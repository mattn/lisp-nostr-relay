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

(ql:quickload '(:websocket-driver :clack :clack-handler-hunchentoot :yason :postmodern :ironclad :babel :alexandria :secp256k1 :bordeaux-threads :split-sequence :log4cl) :silent t)

;; Try to load secp256k1 if available
(handler-case
    (ql:quickload :secp256k1 :silent t)
  (error (e)
    (log:warn "secp256k1 not available, signature verification disabled: ~A" e)))

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

;; Configure log4cl with date format
(log4cl:remove-all-appenders log4cl:*root-logger*)
(log4cl:add-appender log4cl:*root-logger*
                     (make-instance 'log4cl:console-appender
                                    :layout (make-instance 'log4cl:pattern-layout
                                                           :conversion-pattern "%d{%Y-%m-%d %H:%M:%S} [%p] %m%n")))
(log4cl:log-config :info)

(defun configure-log-level ()
  "Set the log4cl root level from the LOG_LEVEL env var (default info).
   Read at runtime (from MAIN) so it takes effect in a dumped
   save-lisp-and-die image, where top-level forms do not re-run."
  (let* ((name (string-downcase (or (uiop:getenv "LOG_LEVEL") "info")))
         (level (cond ((string= name "error") :error)
                      ((string= name "warn")  :warn)
                      ((string= name "info")  :info)
                      ((string= name "debug") :debug)
                      ((string= name "trace") :trace)
                      (t :info))))
    (log4cl:log-config level)
    level))

(defparameter *handler* :hunchentoot)
(defparameter *hunchentoot-settings*
  '(:thread-pool-size 200
    :max-thread-count 200
    :max-accept-count 100))

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
          (log:info "Connecting to database...")
          (let ((conn (connect (getf *db-config* :database)
                               (getf *db-config* :user)
                               (getf *db-config* :password)
                               (getf *db-config* :host)
                               :port (getf *db-config* :port)
                               :pooled-p nil
                               :use-ssl :try)))
            (setf postmodern:*database* conn)
            (log:info "Database connected successfully")
            t))
      (error (e)
        (log:error "Error connecting to database: ~A" e)
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
    (log:info "Database connection lost, attempting to reconnect...")
    (ignore-errors
      (handler-case
          (disconnect-toplevel)
        (error (e)
          (log:warn "Error during disconnect: ~A" e)
          ;; Force cleanup even if disconnect fails
          (ignore-errors (sb-ext:gc :full t)))))
    (dotimes (i *db-max-retries*)
      (log:info "Reconnection attempt ~A/~A" (1+ i) *db-max-retries*)
      (when (connect-db)
        (return-from ensure-db-connection t))
      (unless (= i (1- *db-max-retries*))
        (log:info "Waiting ~A seconds before retry..." *db-retry-delay*)
        (sleep *db-retry-delay*)))
    (log:error "Failed to reconnect after ~A attempts" *db-max-retries*)
    nil))

;; Execute query with automatic reconnection
;;(defmacro with-db-retry (&body body)
;;
;;  "Execute body with automatic reconnection on database error"
;;  (let ((retry-sym (gensym "RETRY"))
;;        (e-sym (gensym "E")))
;;    `(block with-db-retry
;;       (dotimes (,retry-sym 2)
;;         (handler-case
;;             (return-from with-db-retry
;;               (progn ,@body))
;;           (error (,e-sym)
;;             (format t "[INFO] Database error: ~A~%" ,e-sym)
;;             (when (= ,retry-sym 0)
;;               (if (ensure-db-connection)
;;                   (format t "[INFO] Reconnected, retrying query...~%")
;;                   (error "Database reconnection failed"))))))
;;       (error "Query failed after reconnection attempt"))))
(defmacro with-db-retry (&body body)
  "Execute body with automatic reconnection on database error, attempt rollback on error."
  (let ((retry-sym (gensym "RETRY"))
        (e-sym (gensym "E")))
    `(block with-db-retry
       (dotimes (,retry-sym 2)
         (handler-case
             (return-from with-db-retry
               (progn ,@body))
           (error (,e-sym)
             (log:error "Database error: ~A" ,e-sym)
             (ignore-errors (when (fboundp 'rollback) (rollback)))
             (when (= ,retry-sym 0)
               (if (ensure-db-connection)
                   (log:info "Reconnected, retrying query...")
                   (error "Database reconnection failed"))))))
       (error "Query failed after reconnection attempt"))))


;;(defmacro db-execute (query &rest params)
;;  "Execute query with automatic reconnection"
;;  `(with-db-retry
;;     (execute ,query ,@params)))
(defmacro db-execute (query &rest params)
  "Execute query with automatic reconnection and query lock to avoid concurrent execute/query issues."
  `(bordeaux-threads:with-lock-held (*db-query-lock*)
     (with-db-retry
       (execute ,query ,@params))))

;; Query with automatic reconnection
(defmacro db-query (sql-query &rest params)
  "Query with automatic reconnection"
  `(bordeaux-threads:with-lock-held (*db-query-lock*)
     (with-db-retry
       (query ,sql-query ,@params))))

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
(defvar *subscriptions-lock* (bordeaux-threads:make-lock "subscriptions"))
(defvar *clients* nil)
(defvar *clients-lock* (bordeaux-threads:make-lock "clients"))
(defvar *max-connections* 100)
(defvar *connection-count* 0)
(defvar *ws-send-locks* (make-hash-table :test 'eq))
(defvar *ws-send-locks-lock* (bordeaux-threads:make-lock "ws-send-locks"))
(defvar *ws-meta* (make-hash-table :test 'eq))
(defvar *ws-meta-lock* (bordeaux-threads:make-lock "ws-meta"))
(defvar *next-ws-id* 0)

(defparameter *ws-ping-interval*
  (parse-integer (or (uiop:getenv "WS_PING_INTERVAL") "30")))
(defparameter *ws-pong-timeout*
  (parse-integer (or (uiop:getenv "WS_PONG_TIMEOUT") "90")))
;; Wall-clock cap (seconds) for a single WebSocket send/close. A stale half-open
;; client whose socket buffer is full would otherwise block the sending thread
;; (and the per-ws send lock) forever, stalling all broadcasts. On expiry the
;; send is aborted and the subscription is treated as dead.
(defparameter *ws-send-timeout*
  (parse-integer (or (uiop:getenv "WS_SEND_TIMEOUT") "5")))

(defun unix-time ()
  "Return the current UNIX time in seconds."
  (- (get-universal-time) 2208988800))

(defun get-ws-send-lock (ws)
  "Get or create a per-WebSocket send lock to prevent concurrent frame writes."
  (bordeaux-threads:with-lock-held (*ws-send-locks-lock*)
    (or (gethash ws *ws-send-locks*)
        (setf (gethash ws *ws-send-locks*)
              (bordeaux-threads:make-lock "ws-send")))))

(defun remove-ws-send-lock (ws)
  "Remove the send lock for a disconnected WebSocket."
  (bordeaux-threads:with-lock-held (*ws-send-locks-lock*)
    (remhash ws *ws-send-locks*)))

(defun register-ws (ws env)
  "Register metadata for a WebSocket connection and return it."
  (let* ((headers (getf env :headers))
         (peer (or (and headers (gethash "x-forwarded-for" headers))
                   (getf env :remote-addr)
                   (getf env :server-addr)
                   "unknown"))
         (now (unix-time))
         (meta (bordeaux-threads:with-lock-held (*ws-meta-lock*)
                 (let ((id (incf *next-ws-id*)))
                   (setf (gethash ws *ws-meta*)
                         (list :id id
                               :path (getf env :path-info)
                               :peer peer
                               :user-agent (and headers (gethash "user-agent" headers))
                               :connected-at now
                               :last-seen now
                               :last-pong now))))))
    meta))

(defun ws-meta (ws)
  "Get metadata for a WebSocket."
  (bordeaux-threads:with-lock-held (*ws-meta-lock*)
    (gethash ws *ws-meta*)))

(defun unregister-ws (ws)
  "Remove metadata for a WebSocket."
  (bordeaux-threads:with-lock-held (*ws-meta-lock*)
    (remhash ws *ws-meta*)))

(defun update-ws-meta (ws key value)
  "Update a metadata entry for WS."
  (bordeaux-threads:with-lock-held (*ws-meta-lock*)
    (let ((meta (gethash ws *ws-meta*)))
      (when meta
        (setf (getf meta key) value)
        meta))))

(defun ws-log-prefix (ws &optional meta)
  "Format a stable log prefix for a WebSocket."
  (let ((meta (or meta (ws-meta ws))))
    (if meta
        (format nil "ws#~A peer=~A path=~A"
                (getf meta :id)
                (getf meta :peer)
                (getf meta :path))
        (format nil "ws@~X" (sxhash ws)))))

(defun cleanup-ws-state (ws cause &key details)
  "Remove all state associated with WS and emit a summary log."
  (let ((meta (ws-meta ws))
        (ready-state (websocket-driver:ready-state ws))
        (removed-subscriptions 0)
        (removed-subscription-ids 0)
        (remaining-clients 0))
    (bordeaux-threads:with-lock-held (*subscriptions-lock*)
      (maphash (lambda (sub-id sub-list)
                 (let* ((before (length sub-list))
                        (updated-list (remove-if (lambda (sub) (eq (getf sub :ws) ws)) sub-list))
                        (removed-count (- before (length updated-list))))
                   (when (> removed-count 0)
                     (incf removed-subscriptions removed-count)
                     (when (null updated-list)
                       (incf removed-subscription-ids)))
                   (if updated-list
                       (setf (gethash sub-id *subscriptions*) updated-list)
                       (remhash sub-id *subscriptions*))))
               *subscriptions*))
    (bordeaux-threads:with-lock-held (*clients-lock*)
      (setf *clients* (remove ws *clients* :test #'eq))
      (setf *connection-count* (length *clients*))
      (setf remaining-clients *connection-count*))
    (remove-ws-send-lock ws)
    (unregister-ws ws)
    (log:info "~A cleanup cause=~A state=~A removed-subs=~A removed-sub-ids=~A remaining=~A~@[ details=~A~]"
              (ws-log-prefix ws meta)
              cause
              ready-state
              removed-subscriptions
              removed-subscription-ids
              remaining-clients
              details)))

(defmacro with-send-timeout (&body body)
  "Run BODY under *ws-send-timeout* wall-clock seconds. SB-EXT:TIMEOUT is a
   SERIOUS-CONDITION, not an ERROR, so it would slip past callers' (error (e) ...)
   handlers; re-signal it as a plain ERROR so dead-client cleanup runs."
  `(handler-case
       (sb-ext:with-timeout *ws-send-timeout* ,@body)
     (sb-ext:timeout ()
       (error "WebSocket send timed out after ~As" *ws-send-timeout*))))

(defun ws-send (ws message)
  "Thread-safe WebSocket send. Serializes frame writes to prevent interleaved bytes."
  (when (eq (websocket-driver:ready-state ws) :open)
    (with-send-timeout
      (bordeaux-threads:with-lock-held ((get-ws-send-lock ws))
        (when (eq (websocket-driver:ready-state ws) :open)
          (websocket-driver:send ws message))))))

(defun ws-send-ping (ws)
  "Send a ping frame and record the pong when it arrives."
  (when (eq (websocket-driver:ready-state ws) :open)
    (with-send-timeout
      (bordeaux-threads:with-lock-held ((get-ws-send-lock ws))
        (when (eq (websocket-driver:ready-state ws) :open)
          (websocket-driver:send-ping
           ws
           (babel:string-to-octets (format nil "~A" (unix-time)) :encoding :utf-8)
           (lambda ()
             (update-ws-meta ws :last-pong (unix-time)))))))))

(defun ws-close (ws &optional (reason "heartbeat timeout") (code 1001))
  "Close WS while holding the send lock to avoid interleaving with sends."
  (with-send-timeout
    (bordeaux-threads:with-lock-held ((get-ws-send-lock ws))
      (websocket-driver:close-connection ws reason code))))

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
              (log:error "Failed to parse x-only pubkey")
              (return-from verify-schnorr-signature nil))
            ;; Verify schnorr signature
            (let ((verify-result (secp256k1::secp256k1-schnorrsig-verify ctx sig-ptr msg-ptr 32 xonly-pubkey)))
              (= verify-result 1)))))
    (error (e)
      (log:error "Schnorr verification error: ~A" e)
      nil)))

;; Event verification
(defun verify-event (event)
  "Verify Nostr event signature and ID"
  (handler-case
      (let ((id (event-field "id" event))
            (pubkey (event-field "pubkey" event))
            (sig (event-field "sig" event))
            (computed-id (compute-event-id event)))
        (log:info "Event ID: ~A" id)
        (log:info "Computed ID: ~A" computed-id)
        ;; Verify ID
        (unless (string= id computed-id)
          (log:error "Invalid event ID")
          (return-from verify-event nil))
        ;; Verify signature
        (unless (verify-schnorr-signature pubkey id sig)
          (log:error "Invalid signature")
          (return-from verify-event nil))
        t)
    (error (e)
      (log:error "Event verification error: ~A" e)
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
             (log:info "Deletion event, not storing: ~A" id))

            ;; Ephemeral events are not stored
            ((is-ephemeral kind)
             (log:info "Ephemeral event, not storing: ~A" id))

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
          (log:error "Error storing event: ~A" e))))))

(defun match-filter (event filter)
  "Check if event matches filter"
  (let ((event-id (event-field "id" event))
        (event-kind (event-field "kind" event))
        (event-pubkey (event-field "pubkey" event))
        (event-created-at (event-field "created_at" event))
        (event-tags (event-field "tags" event))
        (filter-ids (event-field "ids" filter))
        (filter-kinds (event-field "kinds" filter))
        (filter-authors (event-field "authors" filter))
        (filter-since (event-field "since" filter))
        (filter-until (event-field "until" filter)))
    ;; Check ids filter
    (when (and filter-ids
               (not (some (lambda (fid)
                            (and (stringp fid) (stringp event-id)
                                 (eql 0 (search fid event-id))))
                          (if (listp filter-ids) filter-ids (list filter-ids)))))
      (return-from match-filter nil))
    ;; Check kinds filter
    (when (and filter-kinds (not (member event-kind
                                         (if (listp filter-kinds) filter-kinds (list filter-kinds))
                                         :test #'equal)))
      (return-from match-filter nil))
    ;; Check authors filter (prefix match)
    (when (and filter-authors
               (not (some (lambda (author)
                            (and (stringp author) (stringp event-pubkey)
                                 (eql 0 (search author event-pubkey))))
                          (if (listp filter-authors) filter-authors (list filter-authors)))))
      (return-from match-filter nil))
    ;; Check since filter
    (when (and filter-since event-created-at
               (< event-created-at filter-since))
      (return-from match-filter nil))
    ;; Check until filter
    (when (and filter-until event-created-at
               (> event-created-at filter-until))
      (return-from match-filter nil))
    ;; Check tag filters (#e, #p, etc.)
    (let ((tag-list (when event-tags
                      (if (vectorp event-tags) (coerce event-tags 'list) event-tags))))
      (dolist (pair filter)
        (when (consp pair)
          (let ((key (car pair))
                (values (cdr pair)))
            (when (and (stringp key) (> (length key) 1) (char= (char key 0) #\#))
              (let ((tag-name (subseq key 1)))
                (unless (and values (listp values)
                             (some (lambda (fval)
                                     (some (lambda (tag)
                                             (let ((tag-item (if (vectorp tag) (coerce tag 'list) tag)))
                                               (and (listp tag-item)
                                                    (>= (length tag-item) 2)
                                                    (let ((tn (first tag-item)))
                                                      (equal (if (stringp tn) tn (string-downcase (string tn)))
                                                             tag-name))
                                                    (equal (let ((tv (second tag-item)))
                                                             (if (stringp tv) tv (princ-to-string tv)))
                                                           fval))))
                                           tag-list))
                                   values))
                  (return-from match-filter nil))))))))
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

(defun unregister-client (client)
  (bordeaux-threads:with-lock-held (*clients-lock*)
    (setf *clients* (remove client *clients*))))

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
            (log:debug "SQL: ~A" sql)
            (log:debug "Params: ~A" params)
            (log:debug "Param count: ~A, Expected: ~A" (length params) limit-param-num)
            (let ((results (bordeaux-threads:with-lock-held (*db-query-lock*)
                             (with-db-retry
                               (eval `(query ,sql ,@params))))))
              (log:debug "Results count: ~A" (length results))
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
                                             (log:error "Error parsing tags: ~A" e)
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
                    ;; Check if event is expired (NIP-40)
                    (unless (is-expired event-alist)
                      (log:debug "Sending event: ~A" id)
                      (ws-send ws (encode-json-string (vector "EVENT" subscription-id event-hash))))))))))
        ;; Send EOSE
        (log:debug "Sending EOSE for ~A" subscription-id)
        (ws-send ws (encode-json-string (vector "EOSE" subscription-id)))
        ;; Save subscription (support multiple clients with same sub-id)
        (bordeaux-threads:with-lock-held (*subscriptions-lock*)
          (let ((existing (gethash subscription-id *subscriptions*)))
            (setf (gethash subscription-id *subscriptions*)
                  (cons (list :ws ws :filters filters)
                        (remove-if (lambda (sub) (eq (getf sub :ws) ws)) existing))))))
    (error (e)
      (log:error "Error handling REQ: ~A" e)
      (ws-send ws (encode-json-string (vector "EOSE" subscription-id))))))

(defun handle-deletion-event (event)
  "Handle kind 5 deletion events (NIP-09)"
  (let ((pubkey (event-field "pubkey" event))
        (tags (event-field "tags" event)))
    (when tags
      (let ((tag-list (if (vectorp tags) (coerce tags 'list) tags)))
        (dolist (tag tag-list)
          (let* ((tag-item (if (vectorp tag) (coerce tag 'list) tag)))
            (when (and (listp tag-item) (>= (length tag-item) 2))
              (handler-case
                  (let* ((tag-name-raw (first tag-item))
                         (tag-name (if (stringp tag-name-raw)
                                       tag-name-raw
                                       (string-downcase (string tag-name-raw))))
                         (tag-value-raw (second tag-item))
                         (tag-value (if (stringp tag-value-raw)
                                        tag-value-raw
                                        (princ-to-string tag-value-raw))))
                    (cond
                      ((string= tag-name "e")
                       (handler-case
                           (let ((target-event (db-query "SELECT kind, pubkey, tags::text FROM event WHERE id = $1" tag-value)))
                             (when target-event
                               (destructuring-bind (kind target-pubkey tags-str) (first target-event)
                                 (let ((parsed-tags (when tags-str
                                                      (handler-case
                                                          (yason:parse tags-str :object-as :alist)
                                                        (error () nil)))))
                                   (cond
                                     ((string= target-pubkey pubkey)
                                      (log:info "Deleting event ~A by ~A (standard)" tag-value pubkey)
                                      (db-execute "DELETE FROM event WHERE id = $1 AND pubkey = $2" tag-value pubkey))
                                     ((and (= kind 1059)
                                           parsed-tags
                                           (some (lambda (ptag) (and (listp ptag)
                                                                     (>= (length ptag) 2)
                                                                     (equal (first ptag) "p")
                                                                     (equal (second ptag) pubkey)))
                                                 parsed-tags))
                                      (log:info "Deleting gift wrap event ~A by ~A (NIP-59)" tag-value pubkey)
                                      (db-execute "DELETE FROM event WHERE id = $1" tag-value))
                                     (t
                                      (log:warn "Cannot delete event ~A: no permission" tag-value)))))))
                         (error (e)
                           (log:error "Error processing deletion for ~A: ~A" tag-value e))))
                      ((string= tag-name "a")
                       (let ((parts (split-sequence:split-sequence #\: tag-value)))
                         (when (= (length parts) 3)
                           (let ((kind (parse-integer (first parts) :junk-allowed t))
                                 (target-pubkey (second parts))
                                 (d-tag (third parts)))
                             (when (and kind (string= target-pubkey pubkey))
                               (log:info "Deleting parameterized event ~A:~A:~A" kind pubkey d-tag)
                               (db-execute "DELETE FROM event WHERE kind = $1 AND pubkey = $2 AND $3 = ANY(tagvalues)"
                                           kind pubkey d-tag))))))))
                (error (e)
                  (log:error "ERROR in tag processing: ~A" e))))))))))


(defun get-expiration-timestamp (event)
  "Extract expiration timestamp from event tags (NIP-40)"
  (let ((tags (event-field "tags" event)))
    (when tags
      (let ((tag-list (if (vectorp tags) (coerce tags 'list) tags)))
        (dolist (tag tag-list)
          (let* ((tag-item (if (vectorp tag) (coerce tag 'list) tag)))
            (when (and (listp tag-item) (>= (length tag-item) 2))
              (let* ((tag-name-raw (first tag-item))
                     (tag-name (if (stringp tag-name-raw)
                                   tag-name-raw
                                   (string-downcase (string tag-name-raw))))
                     (tag-value-raw (second tag-item)))
                (when (string= tag-name "expiration")
                  (let ((timestamp (if (integerp tag-value-raw)
                                       tag-value-raw
                                       (parse-integer (princ-to-string tag-value-raw) :junk-allowed t))))
                    (return-from get-expiration-timestamp timestamp)))))))))))

(defun is-expired (event)
  "Check if event is expired according to NIP-40"
  (let ((expiration (get-expiration-timestamp event)))
    (when expiration
      (let ((current-time (get-universal-time)))
        ;; Unix timestamp to Universal time conversion: add 2208988800
        (< expiration (- current-time 2208988800))))))

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
    (log:debug "Storing event: ~A" event)
    ;; Check for expired event (NIP-40)
    (when (is-expired event)
      (let ((event-id (event-field "id" event)))
        (log:info "Rejecting expired event (NIP-40): ~A" event-id)
        (ws-send ws (encode-json-string (vector "OK" event-id yason:false "invalid: event has expired (NIP-40)")))
        (return-from handle-event)))
    ;; Check for protected event (NIP-70)
    (when (has-protected-tag event)
      (let ((event-id (event-field "id" event)))
        (log:info "Rejecting protected event (NIP-70): ~A" event-id)
        (ws-send ws (encode-json-string (vector "OK" event-id yason:false "blocked: event contains '-' tag (NIP-70)")))
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
            (log:debug "Sending OK for event: ~A" event-id)
            (handler-case
                (ws-send ws (encode-json-string (vector "OK" event-id t "")))
              (error (e)
                (log:warn "Failed to send OK response: ~A" e))))
          ;; Broadcast to subscribed clients
          ;; Build event hash once for reuse
          (let ((event-hash (make-hash-table :test 'equal)))
            (dolist (pair event)
              (setf (gethash (car pair) event-hash) (cdr pair)))
            ;; Collect matching subscriptions under lock
            (let ((broadcast-targets nil))
              (bordeaux-threads:with-lock-held (*subscriptions-lock*)
                (maphash (lambda (sub-id sub-list)
                           (dolist (sub-info sub-list)
                             (let ((sub-ws (getf sub-info :ws))
                                   (filters (getf sub-info :filters)))
                               (when (and (eq (websocket-driver:ready-state sub-ws) :open)
                                          (some (lambda (filter) (match-filter event filter)) filters))
                                 (push (list sub-id sub-ws) broadcast-targets)))))
                         *subscriptions*))
              ;; Send to each target with individual error handling
              (let ((dead-pairs nil)
                    (event-id (event-field "id" event)))
                (dolist (target broadcast-targets)
                  (destructuring-bind (sub-id sub-ws) target
                    (handler-case
                        (progn
                          (log:debug "Broadcasting event to subscription: ~A" sub-id)
                          (ws-send sub-ws (encode-json-string (vector "EVENT" sub-id event-hash))))
                      (error (e)
                        (log:warn "Failed to broadcast to subscription ~A: ~A" sub-id e)
                        (push (list sub-id sub-ws) dead-pairs)))))
                ;; One-line INFO summary so streaming is observable without DEBUG.
                (log:info "Broadcast ~A to ~A subscription(s)~@[, ~A dead dropped~]"
                          event-id (length broadcast-targets)
                          (and dead-pairs (length dead-pairs)))
                ;; Clean up dead subscriptions
                (when dead-pairs
                  (bordeaux-threads:with-lock-held (*subscriptions-lock*)
                    (dolist (dead dead-pairs)
                      (destructuring-bind (sub-id sub-ws) dead
                        (let ((existing (gethash sub-id *subscriptions*)))
                          (let ((updated (remove-if (lambda (sub) (eq (getf sub :ws) sub-ws)) existing)))
                            (if updated
                                (setf (gethash sub-id *subscriptions*) updated)
                                (remhash sub-id *subscriptions*))))))))))))

        ;; Verification failed
        (let ((event-id (event-field "id" event)))
          (log:warn "Event verification failed: ~A" event-id)
          (ws-send ws (encode-json-string (vector "OK" event-id yason:false "invalid: signature verification failed")))))))

(defun handle-close (subscription-id ws)
  "Handle CLOSE message"
  (bordeaux-threads:with-lock-held (*subscriptions-lock*)
    (let ((existing (gethash subscription-id *subscriptions*)))
      (setf (gethash subscription-id *subscriptions*)
            (remove-if (lambda (sub) (eq (getf sub :ws) ws)) existing))
      (when (null (gethash subscription-id *subscriptions*))
        (remhash subscription-id *subscriptions*))))
  (log:info "~A client requested CLOSE sub-id=~A remaining-sub-ids=~A"
            (ws-log-prefix ws)
            subscription-id
            (hash-table-count *subscriptions*)))

(defun handle-nostr-message (ws message)
  "Handle Nostr message"
  (handler-case
      (let ((msg (yason:parse message :object-as :alist)))
        ;; Full payloads are large (REQ filters can hold hundreds of ids); keep
        ;; them at DEBUG so INFO stays a readable signal and rare WARN/ERROR
        ;; lines are not flushed out of the container log ring by the noise.
        (log:debug "Received message: ~A" message)
        (log:debug "Parsed as: ~A" msg)
        (when (and (listp msg) (> (length msg) 0))
          (let ((type (first msg)))
            (log:debug "Message type: ~A" type)
            (cond
              ((equal type "EVENT")
               (when (>= (length msg) 2)
                 (log:debug "Handling EVENT")
                 (handle-event ws (second msg))))
              ((equal type "REQ")
               (when (>= (length msg) 2)
                 (let ((sub-id (second msg))
                       (filters (cddr msg)))
                   (log:info "Handling REQ: sub-id=~A filters=~A" sub-id (length filters))
                   (log:debug "REQ ~A filters=~A" sub-id filters)
                   (handle-req ws sub-id filters))))
              ((equal type "CLOSE")
               (when (>= (length msg) 2)
                 (log:info "Handling CLOSE: sub-id=~A" (second msg))
                 (handle-close (second msg) ws)))))))
    (error (e)
      (log:error "Error processing message: ~A" e))))

(defvar *public-path* nil)

(defvar *app*
  (lambda (env)
    (handler-case
        (let ((upgrade (gethash "upgrade" (getf env :headers)))
              (accept (gethash "accept" (getf env :headers)))
              (path (getf env :path-info)))
          (log:info "Request path: ~A, Accept: ~A" path accept)
          (if (and upgrade (string-equal upgrade "websocket"))
              ;; WebSocket connection
              (bordeaux-threads:with-lock-held (*clients-lock*)
                (if (>= *connection-count* *max-connections*)
                    ;; Too many connections
                    '(503 (:content-type "text/plain") ("Service Unavailable: Too many connections"))
                    ;; Accept connection
                    (let ((ws (make-server env)))
                      (register-ws ws env)
                      (incf *connection-count*)
                      (push ws *clients*)
                      (log:info "~A accepted connection total=~A"
                                (ws-log-prefix ws)
                                *connection-count*)
                      (on :message ws
                          (lambda (message)
                            (handler-case
                                (progn
                                  (update-ws-meta ws :last-seen (unix-time))
                                  (log:debug "~A received frame bytes=~A state=~A"
                                             (ws-log-prefix ws)
                                             (length message)
                                             (websocket-driver:ready-state ws))
                                  (handle-nostr-message ws message)
                                  ;; Explicitly allow GC of message
                                  (setf message nil))
                              (error (e)
                                (log:error "~A ERROR in WebSocket message handler: ~A"
                                           (ws-log-prefix ws) e)))))
                      (on :close ws
                          (lambda (&key code reason)
                            (handler-case
                                (progn
                                  (log:info "~A close event code=~A reason=~S state=~A"
                                            (ws-log-prefix ws)
                                            code
                                            reason
                                            (websocket-driver:ready-state ws))
                                  (cleanup-ws-state ws :close
                                                    :details (format nil "code=~A reason=~S" code reason)))
                              (error (e)
                                (log:error "~A Error in close handler: ~A"
                                           (ws-log-prefix ws) e)))))
                      (on :error ws
                          (lambda (error)
                            (log:error "~A WebSocket error: ~A state=~A"
                                       (ws-log-prefix ws)
                                       error
                                       (websocket-driver:ready-state ws))
                            (handler-case
                                (progn
                                  (cleanup-ws-state ws :error
                                                    :details (princ-to-string error)))
                              (error (e)
                                (log:error "~A Error in error handler: ~A"
                                           (ws-log-prefix ws) e)))))
                      (lambda (responder)
                        (declare (ignore responder))
                        (log:info "~A starting websocket session" (ws-log-prefix ws))
                        (start-connection ws)))))
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
                     (log:error "Error serving file for path ~A: ~A" path e)
                     '(400 () ("Bad Request"))))))))
      (error (e)
        (log:error "ERROR in app handler: ~A~%~A" e
                (with-output-to-string (s)
                  (sb-debug:print-backtrace :stream s :count 20)))
        '(500 (:content-type "text/plain") ("Internal Server Error"))))))

(defun cleanup-thread ()
  "Periodic cleanup thread to prevent memory leaks"
  (loop
    (sleep 60) ; 1 minute - more frequent cleanup
    (handler-case
        (progn
          ;; Clean up dead WebSocket connections
          (let ((dead-clients nil))
            (bordeaux-threads:with-lock-held (*clients-lock*)
              (setf dead-clients (remove-if (lambda (ws)
                                              (eq (websocket-driver:ready-state ws) :open))
                                            *clients*))
              (setf *clients* (remove-if (lambda (ws)
                                           (not (eq (websocket-driver:ready-state ws) :open)))
                                         *clients*))
              (setf *connection-count* (length *clients*)))
            (dolist (ws dead-clients)
              (remove-ws-send-lock ws)))
          ;; Clean up orphaned subscriptions
          (let ((active-ws (bordeaux-threads:with-lock-held (*clients-lock*)
                             (copy-list *clients*))))
            (bordeaux-threads:with-lock-held (*subscriptions-lock*)
              (maphash (lambda (sub-id sub-list)
                         (let ((valid-subs (remove-if-not
                                             (lambda (sub)
                                               (member (getf sub :ws) active-ws :test #'eq))
                                             sub-list)))
                           (if valid-subs
                               (setf (gethash sub-id *subscriptions*) valid-subs)
                               (remhash sub-id *subscriptions*))))
                       *subscriptions*)))
          ;; Force garbage collection
          (sb-ext:gc :full t)
          (log:info "Cleanup: connections=~A subs=~A" *connection-count* (hash-table-count *subscriptions*)))
      (error (e)
        (log:error "Error in cleanup thread: ~A" e)))))

(defun heartbeat-thread ()
  "Keep WebSocket connections alive and close stale ones."
  (loop
    (sleep *ws-ping-interval*)
    (handler-case
        (let ((clients (bordeaux-threads:with-lock-held (*clients-lock*)
                         (copy-list *clients*)))
              (now (unix-time)))
          (dolist (ws clients)
            (let ((meta (ws-meta ws)))
              (when (and meta (eq (websocket-driver:ready-state ws) :open))
                (let ((last-activity (max (or (getf meta :last-pong) 0)
                                          (or (getf meta :last-seen) 0)
                                          (or (getf meta :connected-at) 0))))
                  (if (>= (- now last-activity) *ws-pong-timeout*)
                      (progn
                        (log:warn "~A heartbeat timeout last-activity=~A timeout=~A"
                                  (ws-log-prefix ws meta)
                                  last-activity
                                  *ws-pong-timeout*)
                        (ignore-errors (ws-close ws "heartbeat timeout" 1001)))
                      (ignore-errors (ws-send-ping ws))))))))
      (error (e)
        (log:error "Error in heartbeat thread: ~A" e)))))

(defun main ()
  ;; Database initialization and server startup
  (configure-log-level)
  (log:info "Starting application")
  (initialize)
  (connect-db)
  (setf *public-path* (merge-pathnames "public/"
                                       (or *load-pathname*
                                           *compile-file-pathname*
                                           (truename ".")
                                           #p"/app/")))
  ;; Start background maintenance threads
  (bordeaux-threads:make-thread #'cleanup-thread :name "cleanup-thread")
  (bordeaux-threads:make-thread #'heartbeat-thread :name "heartbeat-thread")
  (let ((port (parse-integer (or (uiop:getenv "PORT") "5000"))))
    (log:info "Static files path: ~A" *public-path*)
    (log:info "WebSocket heartbeat: interval=~As pong-timeout=~As send-timeout=~As"
              *ws-ping-interval*
              *ws-pong-timeout*
              *ws-send-timeout*)
    (log:info "Starting server on 0.0.0.0:~A" port)
    (clack:clackup *app*
                   :server *handler*
                   :address "0.0.0.0"
                   :port port
                   :use-thread nil
                   :debug nil
                   :server-options *hunchentoot-settings*)
    (loop (sleep 1)
)))
