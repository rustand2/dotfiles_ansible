#!/bin/sh

ACCOUNT=$(find ~/.config/neomutt/accounts -maxdepth 1 -mindepth 1 | grep -o '[^/]*$' | grep -v '\.' | fzf --border --reverse)
echo "source ~/.config/neomutt/accounts/$ACCOUNT" > ~/.config/neomutt/curr_account
