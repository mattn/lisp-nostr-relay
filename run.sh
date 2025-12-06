#!/bin/sh

sbcl --lose-on-corruption --noinform --disable-debugger --non-interactive --noprint --load nostr-relay.lisp --eval '(nostr-relay:main)'
