(declaim (sb-ext:muffle-conditions style-warning warning))
(sb-ext:disable-debugger)

#+sbcl
(sb-sys:enable-interrupt sb-unix:sigint
                        (lambda (&rest args)
                          (declare (ignore args))
                          (sb-ext:quit :unix-status 130)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((home (or (user-homedir-pathname) (pathname "/root/")))
         (ql-local   (merge-pathnames "setup.lisp" (merge-pathnames ".quicklisp/" home)))
         (ql-roswell (merge-pathnames "setup.lisp" (merge-pathnames ".roswell/lisp/quicklisp/" home)))
         (ql-ros     (merge-pathnames "setup.lisp" (merge-pathnames ".ros/quicklisp/" home)))
         (target (cond
                   ((probe-file ql-local) ql-local)
                   ((probe-file ql-roswell) ql-roswell)
                   ((probe-file ql-ros)     ql-ros)
                   (t nil))))
    (if target
        (load target)
        (error nil "setup.lisp not found in ~/.quicklisp/, ~/.roswell/, ~/.ros/"))))

(ql:quickload '(:websocket-driver :clack :clack-handler-hunchentoot :yason :postmodern :ironclad :babel :alexandria :secp256k1 :bordeaux-threads :split-sequence) :silent t)

;; Try to load secp256k1 if available
(handler-case
    (ql:quickload :secp256k1 :silent t)
  (error (e)
    (format t "Warning: secp256k1 not available, signature verification disabled: ~A~%" e)))

(in-package :cl-user)
(defpackage nostr-relay
  (:use :cl
        :websocket-driver
        :postmodern)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held)
  (:export :main))
(in-package :nostr-relay)

(defparameter *handler* :hunchentoot)

;; URL decode
(defun url-decode (string)
  "Decode URL-encoded string"
  (let ((result (make-array (length string) :element-type 'character :fill-pointer 0)))
    (loop for i from 0 below (length string)
          do (let ((char (char string i)))
               (cond
                 ((char= char #\%)
                  (when (< (+ i 2) (length string))
                    (let ((hex (subseq string (1+ i) (+ i 3))))
                      (vector-push (code-char (parse-integer hex :radix 16)) result)
                      (incf i 2))))
                 ((char= char #\+)
                  (vector-push #\Space result))
                 (t
                  (vector-push char result)))))
    (coerce result 'string)))

;; Parse DATABASE_URL
(defun parse-database-url (url)
  "Parse DATABASE_URL and return connection info (postgres://user:pass@host:port/dbname)"
  (when (and url (not (string= url "")))
    (let* ((protocol-end (search "://" url))
           (rest (if protocol-end (subseq url (+ protocol-end 3)) url))
           (at-pos (position #\@ rest))
           (slash-pos (position #\/ rest :start (or at-pos 0)))
           (colon-pos (position #\: rest :start (or at-pos 0)))
           (userinfo (if at-pos (subseq rest 0 at-pos) ""))
           (user-colon (position #\: userinfo))
           (user (url-decode (if user-colon (subseq userinfo 0 user-colon) userinfo)))
           (password (url-decode (if user-colon (subseq userinfo (1+ user-colon)) "")))
           (host-start (if at-pos (1+ at-pos) 0))
           (host-end (or colon-pos slash-pos (length rest)))
           (host (subseq rest host-start host-end))
           (port (if (and colon-pos slash-pos (< colon-pos slash-pos))
                     (parse-integer (subseq rest (1+ colon-pos) slash-pos))
                     5432))
           (dbname (if slash-pos (subseq rest (1+ slash-pos)) "")))
      (list :database dbname
            :user user
            :password password
            :host host
            :port port))))

;; PostgreSQL connection configuration
(defparameter *database-url* nil)
(defparameter *db-config* nil)
(defparameter *db-connection-lock* (bordeaux-threads:make-lock "db-connection"))
(defparameter *db-query-lock* (bordeaux-threads:make-lock "db-query"))
(defparameter *db-max-retries* 3)
(defparameter *db-retry-delay* 5)

;; Database connection
(defun connect-db ()
  (bordeaux-threads:with-lock-held (*db-connection-lock*)
    (handler-case
        (progn
          (format t "Connecting to database...~%")
          (connect-toplevel (getf *db-config* :database)
                            (getf *db-config* :user)
                            (getf *db-config* :password)
                            (getf *db-config* :host)
                            :port (getf *db-config* :port)
                            :use-ssl :try)
          (format t "Database connected successfully~%")
          t)
      (error (e)
        (format t "Error connecting to database: ~A~%" e)
        nil))))

;; Check if database connection is alive
(defun db-connected-p ()
  (handler-case
      (progn
        (query "SELECT 1" :single)
        t)
    (error () nil)))

;; Reconnect to database with retry logic
(defun ensure-db-connection ()
  (unless (db-connected-p)
    (format t "Database connection lost, attempting to reconnect...~%")
    (disconnect-toplevel)
    (dotimes (i *db-max-retries*)
      (format t "Reconnection attempt ~A/~A~%" (1+ i) *db-max-retries*)
      (when (connect-db)
        (return-from ensure-db-connection t))
      (unless (= i (1- *db-max-retries*))
        (format t "Waiting ~A seconds before retry...~%" *db-retry-delay*)
        (sleep *db-retry-delay*)))
    (format t "Failed to reconnect after ~A attempts~%" *db-max-retries*)
    nil))

;; Execute query with automatic reconnection
(defmacro with-db-retry (&body body)
  "Execute body with automatic reconnection on database error"
  (let ((retry-sym (gensym "RETRY"))
        (e-sym (gensym "E")))
    `(block with-db-retry
       (dotimes (,retry-sym 2)
         (handler-case
             (return-from with-db-retry
               (progn ,@body))
           (error (,e-sym)
             (format t "Database error: ~A~%" ,e-sym)
             (when (= ,retry-sym 0)
               (if (ensure-db-connection)
                   (format t "Reconnected, retrying query...~%")
                   (error "Database reconnection failed"))))))
       (error "Query failed after reconnection attempt"))))

(defmacro db-execute (query &rest params)
  "Execute query with automatic reconnection"
  `(with-db-retry
     (execute ,query ,@params)))

;; Query with automatic reconnection
(defmacro db-query (sql-query &optional params)
  "Query with automatic reconnection"
  `(with-db-retry
     ,(if params
          `(query ,sql-query ,params)
          `(query ,sql-query))))

;; Table creation
(defun initialize ()
  (defparameter *database-url* (uiop:getenv "DATABASE_URL"))
  (defparameter *db-config*
    (or (parse-database-url *database-url*)
        (list :database (or (uiop:getenv "DB_NAME") "lisp-nostr-relay")
              :user (or (uiop:getenv "DB_USER") "postgres")
              :password (or (uiop:getenv "DB_PASSWORD") "")
              :host (or (uiop:getenv "DB_HOST") "localhost")
              :port 5432)))

  (with-connection (list (getf *db-config* :database)
                         (getf *db-config* :user)
                         (getf *db-config* :password)
                         (getf *db-config* :host)
                         :port (getf *db-config* :port)
                         :use-ssl :try)
      (db-execute "CREATE OR REPLACE FUNCTION tags_to_tagvalues(jsonb) RETURNS text[]
                AS 'SELECT array_agg(t->>1) FROM (SELECT jsonb_array_elements($1) AS t)s WHERE length(t->>0) = 1;'
                LANGUAGE SQL
                IMMUTABLE
                RETURNS NULL ON NULL INPUT")
      (db-execute "CREATE TABLE IF NOT EXISTS event (
                id text NOT NULL,
                pubkey text NOT NULL,
                created_at integer NOT NULL,
                kind integer NOT NULL,
                tags jsonb NOT NULL,
                content text NOT NULL,
                sig text NOT NULL,
                tagvalues text[] GENERATED ALWAYS AS (tags_to_tagvalues(tags)) STORED
              )")
      (db-execute "CREATE UNIQUE INDEX IF NOT EXISTS ididx ON event USING btree (id text_pattern_ops)")
      (db-execute "CREATE INDEX IF NOT EXISTS pubkeyprefix ON event USING btree (pubkey text_pattern_ops)")
      (db-execute "CREATE INDEX IF NOT EXISTS timeidx ON event (created_at DESC)")
      (db-execute "CREATE INDEX IF NOT EXISTS kindidx ON event (kind)")
      (db-execute "CREATE INDEX IF NOT EXISTS kindtimeidx ON event(kind,created_at DESC)")
      (db-execute "CREATE INDEX IF NOT EXISTS arbitrarytagvalues ON event USING gin (tagvalues)")))

(defvar *subscriptions* (make-hash-table :test 'equal))
(defvar *clients* nil)
(defvar *clients-lock* (bordeaux-threads:make-lock "clients"))

;; Helper function to get event field (handles both string and keyword keys)
(defun event-field (field event)
  "Get field value from event alist (field can be string or keyword)"
  (let* ((field-str (if (stringp field) field (string field)))
         ;; JSON parser converts each underscore to two hyphens
         (field-key (with-output-to-string (s)
                      (loop for ch across field-str
                            do (if (char= ch #\_)
                                   (princ "--" s)
                                   (princ (char-upcase ch) s))))))
    (or (cdr (assoc (intern field-key :keyword) event))
        (cdr (assoc field event :test #'equal)))))

;; Helper function to encode JSON with yason (supports raw UTF-8)
(defun encode-json-string (obj)
  "Encode object to JSON string using yason (supports raw UTF-8)"
  (with-output-to-string (s)
    (yason:encode obj s)))

;; SHA-256 hash calculation
(defun sha256-hex (string)
  "Return SHA-256 hash of string as hexadecimal string"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256
                             (babel:string-to-octets string :encoding :utf-8))))

;; Compute event ID
(defun compute-event-id (event)
  "Compute Nostr event ID"
  (let* ((pubkey (event-field "pubkey" event))
         (created-at (event-field "created_at" event))
         (kind (event-field "kind" event))
         (tags-raw (event-field "tags" event))
         (tags (if (null tags-raw) (vector) tags-raw))
         (content (event-field "content" event))
         (serialized (encode-json-string 
                      (list 0 pubkey created-at kind tags content))))
    (sha256-hex serialized)))

;; Convert hex string to byte array
(defun hex-to-bytes (hex-string)
  "Convert hexadecimal string to byte array"
  (let* ((len (length hex-string))
         (bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (/ len 2)
          do (setf (aref bytes i)
                   (parse-integer hex-string :start (* i 2) :end (* (1+ i) 2) :radix 16)))
    bytes))

;; Schnorr signature verification (BIP340) using libsecp256k1
(defun verify-schnorr-signature (pubkey message sig)
  "Verify Schnorr signature using secp256k1_schnorrsig_verify"
  (handler-case
      (let ((ctx (secp256k1::ensure-context))
            (pubkey-bytes (hex-to-bytes pubkey))
            (message-bytes (hex-to-bytes message))
            (sig-bytes (hex-to-bytes sig)))
        ;; Check lengths
        (unless (and (= (length pubkey-bytes) 32)
                     (= (length message-bytes) 32)
                     (= (length sig-bytes) 64))
          (return-from verify-schnorr-signature nil))
        ;; Create foreign arrays
        (cffi:with-foreign-objects ((xonly-pubkey '(:struct secp256k1::secp256k1-xonly-pubkey))
                                     (sig-ptr :uchar 64)
                                     (msg-ptr :uchar 32)
                                     (pubkey-ptr :uchar 32))
          ;; Copy bytes to foreign memory
          (loop for i from 0 below 32
                do (setf (cffi:mem-aref pubkey-ptr :uchar i) (aref pubkey-bytes i))
                   (setf (cffi:mem-aref msg-ptr :uchar i) (aref message-bytes i)))
          (loop for i from 0 below 64
                do (setf (cffi:mem-aref sig-ptr :uchar i) (aref sig-bytes i)))
          ;; Parse x-only pubkey
          (let ((parse-result (secp256k1::secp256k1-xonly-pubkey-parse ctx xonly-pubkey pubkey-ptr)))
            (unless (= parse-result 1)
              (format t "Failed to parse x-only pubkey~%")
              (return-from verify-schnorr-signature nil))
            ;; Verify schnorr signature
            (let ((verify-result (secp256k1::secp256k1-schnorrsig-verify ctx sig-ptr msg-ptr 32 xonly-pubkey)))
              (= verify-result 1)))))
    (error (e)
      (format t "Schnorr verification error: ~A~%" e)
      nil)))

;; Event verification
(defun verify-event (event)
  "Verify Nostr event signature and ID"
  (handler-case
      (let ((id (event-field "id" event))
            (pubkey (event-field "pubkey" event))
            (sig (event-field "sig" event))
            (computed-id (compute-event-id event)))
        (format t "Event ID: ~A~%" id)
        (format t "Computed ID: ~A~%" computed-id)
        ;; Verify ID
        (unless (string= id computed-id)
          (format t "Invalid event ID~%")
          (return-from verify-event nil))
        ;; Verify signature
        (unless (verify-schnorr-signature pubkey id sig)
          (format t "Invalid signature~%")
          (return-from verify-event nil))
        t)
    (error (e)
      (format t "Event verification error: ~A~%" e)
      nil)))

(defun is-replaceable (kind)
  "Check if event kind is replaceable (kind 0, 3, or 10000-19999)"
  (or (= kind 0)
      (= kind 3)
      (and (>= kind 10000) (<= kind 19999))))

(defun is-deletion (kind)
  "Check if event kind is deletion (kind 5)"
  (= kind 5))

(defun is-ephemeral (kind)
  "Check if event kind is ephemeral (kind 20000-29999)"
  (and (>= kind 20000) (<= kind 29999)))

(defun is-parameterized-replaceable (kind)
  "Check if event kind is parameterized replaceable (kind 30000-39999)"
  (and (>= kind 30000) (<= kind 39999)))

(defun get-d-tag (tags)
  "Extract 'd' tag value from tags array"
  (when (listp tags)
    (dolist (tag tags)
      (when (and (listp tag) 
                 (>= (length tag) 2)
                 (equal (first tag) "d"))
        (return-from get-d-tag (second tag)))))
  "")

(defun store-event (event)
  "Store event in PostgreSQL"
  (let ((id (event-field "id" event))
        (pubkey (event-field "pubkey" event))
        (created-at (event-field "created_at" event))
        (kind (event-field "kind" event))
        (tags (let ((t-raw (event-field "tags" event)))
                (if (null t-raw) (vector) t-raw)))
        (content (event-field "content" event))
        (sig (event-field "sig" event)))
    (when id
      (handler-case
          (cond
            ;; Deletion events are not stored
            ((is-deletion kind)
             (format t "Deletion event, not storing: ~A~%" id))

            ;; Ephemeral events are not stored
            ((is-ephemeral kind)
             (format t "Ephemeral event, not storing: ~A~%" id))
            
            ;; Replaceable events: replace if newer
            ((is-replaceable kind)
             (db-execute "INSERT INTO event (id, pubkey, created_at, kind, tags, content, sig)
                       VALUES ($1, $2, $3, $4, $5::jsonb, $6, $7)
                       ON CONFLICT (id) DO NOTHING"
                      id pubkey created-at kind 
                      (encode-json-to-string tags)
                      content sig)
             ;; Delete older events with same pubkey and kind
             (db-execute "DELETE FROM event WHERE pubkey = $1 AND kind = $2 AND created_at < $3"
                      pubkey kind created-at))
            
            ;; Parameterized replaceable events: replace if newer with same d tag
            ((is-parameterized-replaceable kind)
             (let ((d-tag (get-d-tag tags)))
               (db-execute "INSERT INTO event (id, pubkey, created_at, kind, tags, content, sig)
                         VALUES ($1, $2, $3, $4, $5::jsonb, $6, $7)
                         ON CONFLICT (id) DO NOTHING"
                        id pubkey created-at kind 
                        (encode-json-string tags)
                        content sig)
               ;; Delete older events with same pubkey, kind, and d tag
               (db-execute "DELETE FROM event 
                         WHERE pubkey = $1 AND kind = $2 AND created_at < $3
                         AND $4 = ANY(tagvalues)"
                        pubkey kind created-at d-tag)))
            
            ;; Regular events
            (t
             (db-execute "INSERT INTO event (id, pubkey, created_at, kind, tags, content, sig)
                       VALUES ($1, $2, $3, $4, $5::jsonb, $6, $7)
                       ON CONFLICT (id) DO NOTHING"
                      id pubkey created-at kind 
                      (encode-json-string tags)
                      content sig)))
        (error (e)
          (format t "Error storing event: ~A~%" e))))))

(defun match-filter (event filter)
  "Check if event matches filter"
  (let ((event-kind (event-field "kind" event))
        (event-pubkey (event-field "pubkey" event))
        (filter-kinds (event-field "kinds" filter))
        (filter-authors (event-field "authors" filter)))
    ;; Check kinds filter
    (when (and filter-kinds (not (member event-kind filter-kinds :test #'equal)))
      (return-from match-filter nil))
    ;; Check authors filter
    (when (and filter-authors (not (member event-pubkey filter-authors :test #'string=)))
      (return-from match-filter nil))
    ;; If we get here, all checks passed
    t))

(defun build-query (filters)
  "Build SQL query from filters with parameterized queries"
  (let ((filter-conditions nil)
        (all-params nil)
        (param-counter 0)
        (max-limit 100))
    (dolist (filter filters)
      (let ((kinds (event-field "kinds" filter))
            (authors (event-field "authors" filter))
            (since (event-field "since" filter))
            (until (event-field "until" filter))
            (limit (event-field "limit" filter))
            (conditions nil))
        (when limit
          (when (and (integerp limit) (> limit 0))
            (setf max-limit (min max-limit limit))))
        (when kinds
          (if (listp kinds)
              (let ((placeholders nil))
                (dolist (kind kinds)
                  (incf param-counter)
                  (push kind all-params)
                  (push (format nil "$~A" param-counter) placeholders))
                (push (format nil "kind IN (~{~A~^,~})" (reverse placeholders)) conditions))
              (progn
                (incf param-counter)
                (push kinds all-params)
                (push (format nil "kind = $~A" param-counter) conditions))))
        (when authors
          (if (listp authors)
              (let ((placeholders nil))
                (dolist (author authors)
                  (incf param-counter)
                  (push author all-params)
                  (push (format nil "$~A" param-counter) placeholders))
                (push (format nil "pubkey IN (~{~A~^,~})" (reverse placeholders)) conditions))
              (progn
                (incf param-counter)
                (push authors all-params)
                (push (format nil "pubkey = $~A" param-counter) conditions))))
        (when since
          (incf param-counter)
          (push since all-params)
          (push (format nil "created_at >= $~A" param-counter) conditions))
        (when until
          (incf param-counter)
          (push until all-params)
          (push (format nil "created_at <= $~A" param-counter) conditions))
        ;; Handle tag filters
        (dolist (pair filter)
          (when (consp pair)
            (let ((key (car pair))
                  (value (cdr pair)))
              (when (stringp key)
                (when (and (> (length key) 1) (char= (char key 0) #\#))
                  (when (and value (listp value))
                    (dolist (tag-value value)
                      (incf param-counter)
                      (push tag-value all-params)
                      (push (format nil "$~A = ANY(tagvalues)" param-counter) conditions))))))))
        (when conditions
          (push (format nil "(~{~A~^ AND ~})" conditions) filter-conditions))))
    ;; Add limit
    (incf param-counter)
    (push max-limit all-params)
    (values filter-conditions (reverse all-params) param-counter)))

(defun handle-req (ws subscription-id filters)
  "Handle REQ message"
  (handler-case
      (progn
        ;; Search from PostgreSQL
        (multiple-value-bind (conditions params limit-param-num)
            (build-query filters)
          (let* ((where-clause (if conditions
                                   (format nil "WHERE ~{~A~^ OR ~}" conditions)
                                   ""))
                 (sql (format nil "SELECT id, pubkey, created_at, kind, tags::text, content, sig FROM event ~A ORDER BY created_at DESC LIMIT $~A" where-clause limit-param-num)))
            (format t "SQL: ~A~%" sql)
            (format t "Params: ~A~%" params)
            (format t "Param count: ~A, Expected: ~A~%" (length params) limit-param-num)
            (let ((results (bordeaux-threads:with-lock-held (*db-query-lock*)
                             (with-db-retry
                               (eval `(query ,sql ,@params))))))
              (format t "Results count: ~A~%" (length results))
              ;; Send matched events
              (dolist (row results)
                (destructuring-bind (id pubkey created-at kind tags content sig) row
                  (let* ((parsed-tags (cond
                                        ((null tags) (vector))
                                        ((stringp tags)
                                         (handler-case
                                             (let* ((tag-bytes (babel:string-to-octets tags :encoding :utf-8))
                                                    (tag-str (babel:octets-to-string tag-bytes :encoding :utf-8))
                                                    (alist-tags (yason:parse tag-str :object-as :alist)))
                                               ;; Convert alist to vector of vectors with all strings
                                               (map 'vector 
                                                    (lambda (tag) 
                                                      (map 'vector 
                                                           (lambda (item)
                                                             (if (stringp item)
                                                                 item
                                                                 (format nil "~(~A~)" item)))
                                                           tag))
                                                    alist-tags))
                                           (error (e) 
                                             (format t "Error parsing tags: ~A~%" e)
                                             (vector))))
                                        ((listp tags) 
                                         (map 'vector 
                                              (lambda (tag) 
                                                (map 'vector 
                                                     (lambda (item)
                                                       (if (stringp item)
                                                           item
                                                           (format nil "~(~A~)" item)))
                                                     tag))
                                              tags))
                                        (t (vector))))
                         (event-alist (list (cons "id" id)
                                            (cons "pubkey" pubkey)
                                            (cons "created_at" created-at)
                                            (cons "kind" kind)
                                            (cons "tags" parsed-tags)
                                            (cons "content" content)
                                            (cons "sig" sig)))
                         (event-hash (let ((ht (make-hash-table :test 'equal)))
                                       (dolist (pair event-alist)
                                         (setf (gethash (car pair) ht) (cdr pair)))
                                       ht)))
                    (format t "Sending event: ~A~%" id)
                    (send ws (encode-json-string (vector "EVENT" subscription-id event-hash)))))))))
        ;; Send EOSE
        (format t "Sending EOSE for ~A~%" subscription-id)
        (send ws (encode-json-string (vector "EOSE" subscription-id)))
        ;; Save subscription (support multiple clients with same sub-id)
        (let ((existing (gethash subscription-id *subscriptions*)))
          (setf (gethash subscription-id *subscriptions*)
                (cons (list :ws ws :filters filters) 
                      (remove-if (lambda (sub) (eq (getf sub :ws) ws)) existing)))))
    (error (e)
      (format t "Error handling REQ: ~A~%" e)
      (send ws (encode-json-string (vector "EOSE" subscription-id))))))

(defun handle-deletion-event (event)
  "Handle kind 5 deletion events (NIP-09)"
  (let ((pubkey (event-field "pubkey" event))
        (tags (event-field "tags" event)))
    (when (listp tags)
      (dolist (tag tags)
        (when (and (listp tag) (>= (length tag) 2))
          (let ((tag-name (first tag))
                (tag-value (second tag)))
            (cond
              ((equal tag-name "e")
               (handler-case
                   (let ((target-event (db-query "SELECT kind, pubkey, tags::text FROM event WHERE id = $1" (list tag-value))))
                     (when target-event
                       (destructuring-bind (kind target-pubkey tags-str) (first target-event)
                         (let ((parsed-tags (when tags-str
                                              (handler-case
                                                  (yason:parse tags-str :object-as :alist)
                                                (error () nil)))))
                           (cond
                             ;; Standard deletion if pubkey matches
                             ((string= target-pubkey pubkey)
                              (format t "Deleting event ~A by ~A (standard)~%" tag-value pubkey)
                              (db-execute "DELETE FROM event WHERE id = $1 AND pubkey = $2" tag-value pubkey))
                             ;; NIP-59: Delete kind 1059 if pubkey matches any p-tag
                             ((and (= kind 1059)
                                   parsed-tags
                                   (some (lambda (ptag) (and (listp ptag)
                                                             (>= (length ptag) 2)
                                                             (equal (first ptag) "p")
                                                             (equal (second ptag) pubkey)))
                                         parsed-tags))
                              (format t "Deleting gift wrap event ~A by ~A (NIP-59)~%" tag-value pubkey)
                              (db-execute "DELETE FROM event WHERE id = $1" tag-value))
                             (t
                              (format t "Cannot delete event ~A: no permission~%" tag-value)))))))
                 (error (e)
                   (format t "Error processing deletion for ~A: ~A~%" tag-value e)
                   nil)))
              ;; Delete events by coordinate (kind:pubkey:d-tag)
              ((equal tag-name "a")
               (let ((parts (split-sequence:split-sequence #\: tag-value)))
                 (when (= (length parts) 3)
                   (let ((kind (parse-integer (first parts) :junk-allowed t))
                         (target-pubkey (second parts))
                         (d-tag (third parts)))
                     (when (and kind (string= target-pubkey pubkey))
                       (format t "Deleting parameterized event ~A:~A:~A~%" kind pubkey d-tag)
                       (db-execute "DELETE FROM event WHERE kind = $1 AND pubkey = $2 AND $3 = ANY(tagvalues)"
                                kind pubkey d-tag))))))
              )))))))

(defun has-protected-tag (event)
  "Check if event has a '-' tag (NIP-70 Protected Events)"
  (let ((tags (cdr (assoc "tags" event :test #'equal))))
    (when (listp tags)
      (some (lambda (tag)
              (and (listp tag)
                   (> (length tag) 0)
                   (equal (first tag) "-")))
            tags))))

(defun handle-event (ws event-data)
  "Handle EVENT message"
  (let ((event event-data))
    (format t "Storing event: ~A~%" event)
    ;; Check for protected event (NIP-70)
    (when (has-protected-tag event)
      (let ((event-id (event-field "id" event)))
        (format t "Rejecting protected event (NIP-70): ~A~%" event-id)
        (send ws (encode-json-string (vector "OK" event-id :false "blocked: event contains '-' tag (NIP-70)")))
        (return-from handle-event)))
    ;; Verify event
    (if (verify-event event)
        (progn
          ;; Handle deletion events (kind 5)
          (when (= (event-field "kind" event) 5)
            (handle-deletion-event event))
          ;; Store event
          (store-event event)
          ;; Send OK response
          (let ((event-id (event-field "id" event)))
            (format t "Sending OK for event: ~A~%" event-id)
            (send ws (encode-json-string (vector "OK" event-id t ""))))
          ;; Broadcast to subscribed clients
          (maphash (lambda (sub-id sub-list)
                     (dolist (sub-info sub-list)
                       (let ((sub-ws (getf sub-info :ws))
                             (filters (getf sub-info :filters)))
                         ;; Check if event matches any filter
                         (when (some (lambda (filter) (match-filter event filter)) filters)
                           (format t "Broadcasting event to subscription: ~A~%" sub-id)
                           ;; Convert alist to hash table for encoding
                           (let ((event-hash (make-hash-table :test 'equal)))
                             (dolist (pair event)
                               (setf (gethash (car pair) event-hash) (cdr pair)))
                             (send sub-ws (encode-json-string (vector "EVENT" sub-id event-hash))))))))
                   *subscriptions*))
        ;; Verification failed
        (let ((event-id (event-field "id" event)))
          (format t "Event verification failed: ~A~%" event-id)
          (send ws (encode-json-string (vector "OK" event-id :false "invalid: signature verification failed")))))))

(defun handle-close (subscription-id ws)
  "Handle CLOSE message"
  (let ((existing (gethash subscription-id *subscriptions*)))
    (setf (gethash subscription-id *subscriptions*)
          (remove-if (lambda (sub) (eq (getf sub :ws) ws)) existing))
    (when (null (gethash subscription-id *subscriptions*))
      (remhash subscription-id *subscriptions*))))

(defun handle-nostr-message (ws message)
  "Handle Nostr message"
  (handler-case
      (let ((msg (yason:parse message :object-as :alist)))
        (format t "Received message: ~A~%" message)
        (format t "Parsed as: ~A~%" msg)
        (when (and (listp msg) (> (length msg) 0))
          (let ((type (first msg)))
            (format t "Message type: ~A~%" type)
            (cond
              ((equal type "EVENT")
               (when (>= (length msg) 2)
                 (format t "Handling EVENT~%")
                 (handle-event ws (second msg))))
              ((equal type "REQ")
               (when (>= (length msg) 2)
                 (let ((sub-id (second msg))
                       (filters (cddr msg)))
                   (format t "Handling REQ: sub-id=~A~%" sub-id)
                   (handle-req ws sub-id filters))))
              ((equal type "CLOSE")
               (when (>= (length msg) 2)
                 (format t "Handling CLOSE~%")
                 (handle-close (second msg) ws)))))))
    (error (e)
      (format t "Error processing message: ~A~%" e))))

(defvar *public-path* nil)

(defvar *app*
  (lambda (env)
    (handler-case
        (let ((upgrade (gethash "upgrade" (getf env :headers)))
              (accept (gethash "accept" (getf env :headers)))
              (path (getf env :path-info)))
          (format t "~A Request path: ~A, Accept: ~A~%" 
                  (get-universal-time) path accept)
          (force-output)
          (if (and upgrade (string-equal upgrade "websocket"))
              ;; WebSocket connection
              (let ((ws (make-server env)))
                (bordeaux-threads:with-lock-held (*clients-lock*)
                  (push ws *clients*))
                (on :message ws
                    (lambda (message)
                      (handler-case
                          (handle-nostr-message ws message)
                        (error (e)
                          (format t "ERROR in WebSocket message handler: ~A~%" e)
                          (force-output)))))
                (on :close ws
                    (lambda (&key code reason)
                      (declare (ignore code reason))
                      (bordeaux-threads:with-lock-held (*clients-lock*)
                        (setf *clients* (remove ws *clients*)))
                      ;; Remove subscriptions for this client
                      (maphash (lambda (sub-id sub-list)
                                 (let ((updated-list (remove-if (lambda (sub) (eq (getf sub :ws) ws)) sub-list)))
                                   (if updated-list
                                       (setf (gethash sub-id *subscriptions*) updated-list)
                                       (remhash sub-id *subscriptions*))))
                               *subscriptions*)
                      ;; Force garbage collection
                      (sb-ext:gc :full t)))
                (lambda (responder)
                  (declare (ignore responder))
                  (start-connection ws)))
              ;; Normal HTTP request
              (cond
                ;; NIP-11 relay information
                ((and accept (search "application/nostr+json" accept))
                 (let* ((relay-name (or (uiop:getenv "RELAY_NAME") "Lisp Nostr Relay"))
                        (relay-description (or (uiop:getenv "RELAY_DESCRIPTION") 
                                               "A lightweight Nostr relay implementation in Common Lisp"))
                        (relay-pubkey (or (uiop:getenv "RELAY_PUBKEY") ""))
                        (relay-contact (or (uiop:getenv "RELAY_CONTACT") ""))
                        (relay-icon (or (uiop:getenv "RELAY_ICON") ""))
                        (info (make-hash-table :test 'equal)))
                   (setf (gethash "name" info) relay-name)
                   (setf (gethash "description" info) relay-description)
                   (when (not (string= relay-pubkey ""))
                     (setf (gethash "pubkey" info) relay-pubkey))
                   (when (not (string= relay-contact ""))
                     (setf (gethash "contact" info) relay-contact))
                   (when (not (string= relay-icon ""))
                     (setf (gethash "icon" info) relay-icon))
                   (setf (gethash "supported_nips" info) 
                         (vector 1 2 4 9 11 12 15 16 20 22 28 33 40 50 62 70))
                   (setf (gethash "software" info) "https://github.com/mattn/lisp-nostr-relay")
                   (setf (gethash "version" info) "1.0.0")
                   (let ((limitation (make-hash-table :test 'equal)))
                     (setf (gethash "max_message_length" limitation) 65536)
                     (setf (gethash "max_subscriptions" limitation) 20)
                     (setf (gethash "max_filters" limitation) 10)
                     (setf (gethash "max_limit" limitation) 500)
                     (setf (gethash "max_subid_length" limitation) 100)
                     (setf (gethash "min_prefix" limitation) 4)
                     (setf (gethash "max_event_tags" limitation) 2000)
                     (setf (gethash "max_content_length" limitation) 65536)
                     (setf (gethash "min_pow_difficulty" limitation) 0)
                     (setf (gethash "auth_required" limitation) yason:false)
                     (setf (gethash "payment_required" limitation) yason:false)
                     (setf (gethash "limitation" info) limitation))
                   (list 200 
                         (list :content-type "application/nostr+json"
                               :access-control-allow-origin "*"
                               :access-control-allow-headers "Content-Type"
                               :access-control-allow-methods "GET")
                         (list (encode-json-string info)))))
                ;; Static files
                (t
                 (handler-case
                     (let* ((safe-path (remove-if (lambda (c)
                                                     (or (char= c #\[) (char= c #\])
                                                         (char= c #\Null)))
                                                   path))
                            (file-path (if (string= safe-path "/")
                                           (merge-pathnames "index.html" *public-path*)
                                           (merge-pathnames (string-left-trim "/" safe-path) *public-path*))))
                       (if (probe-file file-path)
                           (let ((content (alexandria:read-file-into-byte-vector file-path))
                                 (content-type (hunchentoot:mime-type file-path)))
                             (list 200 (list :content-type content-type) content))
                           '(404 () ("Not Found"))))
                   (error (e)
                     (format t "Error serving file for path ~A: ~A~%" path e)
                     (force-output)
                     '(400 () ("Bad Request"))))))))
      (error (e)
        (format t "ERROR in app handler: ~A~%~A~%" e 
                (with-output-to-string (s)
                  (sb-debug:print-backtrace :stream s :count 20)))
        (force-output)
        '(500 (:content-type "text/plain") ("Internal Server Error"))))))

(defun cleanup-thread ()
  "Periodic cleanup thread to prevent memory leaks"
  (loop
    (sleep 300) ; 5 minutes
    (handler-case
        (progn
          ;; Clean up dead WebSocket connections
          (bordeaux-threads:with-lock-held (*clients-lock*)
            (setf *clients* (remove-if-not #'websocket-driver:ready-state *clients*)))
          ;; Clean up orphaned subscriptions
          (let ((active-ws (bordeaux-threads:with-lock-held (*clients-lock*)
                             (copy-list *clients*))))
            (maphash (lambda (sub-id sub-list)
                       (let ((valid-subs (remove-if-not 
                                           (lambda (sub) 
                                             (member (getf sub :ws) active-ws))
                                           sub-list)))
                         (if valid-subs
                             (setf (gethash sub-id *subscriptions*) valid-subs)
                             (remhash sub-id *subscriptions*))))
                     *subscriptions*))
          ;; Force garbage collection
          (sb-ext:gc :full t)
          (format t "Cleanup completed at ~A~%" (get-universal-time)))
      (error (e)
        (format t "Error in cleanup thread: ~A~%" e)))))

(defun main ()
  ;; Database initialization and server startup
  (initialize)
  (connect-db)
  (setf *public-path* (merge-pathnames "public/" 
                                       (or *load-pathname* 
                                           *compile-file-pathname*
                                           (truename ".")
                                           #p"/app/")))
  ;; Start cleanup thread
  (bordeaux-threads:make-thread #'cleanup-thread :name "cleanup-thread")
  (let ((port (parse-integer (or (uiop:getenv "PORT") "5000"))))
    (format t "Static files path: ~A~%" *public-path*)
    (format t "Starting server on 0.0.0.0:~A~%" port)
    (force-output)
    (clack:clackup *app* :server *handler* :address "0.0.0.0" :port port :use-thread t)
    (loop (sleep 1)
)))
