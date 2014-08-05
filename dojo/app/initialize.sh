#!/usr/bin/env sh
openssl genrsa -des3 -out privkey.pem 1024
openssl req -new -x509 -days 1001 -key privkey.pem -out cert.pem

