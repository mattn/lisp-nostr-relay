# Common Lisp Nostr Relay

A lightweight Nostr relay implementation in Common Lisp.

## NIP Support

This relay supports the following Nostr Implementation Possibilities (NIPs):

- **NIP-01**: Basic protocol flow
- **NIP-02**: Contact List and Petnames
- **NIP-09**: Event deletion (kind 5)
- **NIP-11**: Relay Information Document
- **NIP-12**: Generic tag queries (#e, #p, etc)
- **NIP-15**: Marketplace
- **NIP-16**: Event Treatment (ephemeral events 20000-29999)
- **NIP-20**: Command results (OK messages)
- **NIP-22**: Event created_at limits (timestamp validation)
- **NIP-26**: Delegated Event Signing
- **NIP-28**: Public Chat
- **NIP-33**: Parameterized Replaceable Events (kind 30000-39999)
- **NIP-40**: Expiration Timestamp
- **NIP-70**: Protected Events (rejects events with "-" tag)

## Usage

```bash
sbcl --non-interactive --load nostr-relay.lisp --eval '(nostr-relay:main)'
```

Or use the provided script:

```bash
./run.sh
```

The relay will start on `ws://localhost:5000` by default.

## Docker

Build and run with Docker:

```bash
docker build -t nostr-relay .
docker run -p 5000:5000 -e DATABASE_URL='postgres://user:pass@host:5432/dbname' nostr-relay
```

## Installation

```bash
git clone <repository-url>
cd lisp-nostr-relay
```

## Requirements

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- PostgreSQL
- libsecp256k1

Required Lisp libraries (automatically loaded via Quicklisp):
- websocket-driver
- clack
- clack-handler-hunchentoot
- jonathan
- postmodern
- ironclad
- babel
- alexandria
- cl-secp256k1 (install manually: `~/quicklisp/local-projects/`)

## Notes

**Signature Verification**: Currently, Schnorr signature verification is not fully implemented. The relay accepts events based on format validation only. To enable full signature verification, install `libsecp256k1` and `cl-secp256k1`.

## Configuration

Set the following environment variables:

- `DATABASE_URL`: PostgreSQL connection string (e.g., `postgres://user:pass@host:5432/dbname`)
- Or use individual variables: `DB_NAME`, `DB_USER`, `DB_PASSWORD`, `DB_HOST`

## License

MIT License

## Author

Yasuhiro Matsumoto (a.k.a. mattn)
