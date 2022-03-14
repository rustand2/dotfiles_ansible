#!/bin/bash

REAL_NAME=$1
EMAIL=$2
PASSWORD=$3

gpg --gen-key --batch <(cat << EOF
Key-Type: 1
Key-Length: 2048
Subkey-Type: 1
Subkey-Length: 2048
Name-Real: $REAL_NAME
Name-Email: $EMAIL
Expire-Date: 0
Passphrase: $PASSWORD
EOF
)
