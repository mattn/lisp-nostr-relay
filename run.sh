#!/bin/sh

sbcl --non-interactive --load nostr-relay.lisp --eval '(nostr-relay:main)'
