#!/bin/sh

ACCOUNT=$(find ~/.mutt/accounts -maxdepth 1 -mindepth 1 | grep -o '[^/]*$' | grep -v '\.' | fzf --border --reverse)
echo "source ~/.mutt/accounts/$ACCOUNT" > ~/.mutt/curr_account
