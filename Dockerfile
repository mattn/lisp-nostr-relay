FROM fukamachi/sbcl:latest-alpine AS builder

WORKDIR /app
COPY . /app

# Install libsecp256k1
RUN apk add --no-cache \
    libsecp256k1-dev \
    git

# Install cl-secp256k1 to quicklisp local-projects
RUN mkdir -p /root/quicklisp/local-projects && \
    cd /root/quicklisp/local-projects && \
    git clone https://github.com/dvush/cl-secp256k1.git

ENV CLACK_HANDLER="hunchentoot"

RUN sbcl --non-interactive \
    --load /app/nostr-relay.lisp \
    --eval "(sb-ext:save-lisp-and-die \"/app/nostr-relay\" :toplevel #'nostr-relay:main :executable t :compression 9)" \
    --quit

FROM alpine:latest
RUN apk add --no-cache zstd libsecp256k1 && \
    # Create symlink for libsecp256k1.so if needed
    if [ ! -f /usr/lib/libsecp256k1.so ]; then \
        ln -sf /usr/lib/libsecp256k1.so.* /usr/lib/libsecp256k1.so || true; \
    fi
WORKDIR /app
ENV DATABASE_URL='postgres://postgres:password@db:5432/nostr-relay'
ENV LD_LIBRARY_PATH=/usr/lib:/usr/local/lib
COPY --from=builder /app/nostr-relay /app/nostr-relay
COPY --from=builder /app/public /app/public
EXPOSE 5000

ENTRYPOINT ["/app/nostr-relay"]
