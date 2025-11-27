FROM fukamachi/sbcl:latest-alpine AS builder

WORKDIR /app
COPY . /app

ENV CLACK_HANDLER="hunchentoot"

RUN sbcl --non-interactive \
    --load /app/nostr-relay.lisp \
    --eval "(sb-ext:save-lisp-and-die \"/app/nostr-relay\" :toplevel #'nostr-relay:main :executable t :compression 9)" \
    --quit

FROM alpine:latest
RUN apk add --no-cache zstd
WORKDIR /app
ENV DATABASE_URL='postgres://postgres:password@db:5432/nostr-relay'
COPY --from=builder /app/nostr-relay /app/nostr-relay
COPY --from=builder /app/public /app/public
EXPOSE 5000

ENTRYPOINT ["/app/nostr-relay"]
