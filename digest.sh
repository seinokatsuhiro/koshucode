#!/bin/sh

dgst () {
    for m in -md5 -sha1 -ripemd160; do
        /usr/bin/openssl dgst $m "$1"
    done
}

dgst koshu.exe

