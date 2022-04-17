#!/bin/sh

notmuch new

NEW_MAIL="$(notmuch search tag:new)"
while IFS= read -r MESSAGE; do
    DATE="$(echo $MESSAGE | cut -d ' ' -f 2)"
    COUNT="$(echo $MESSAGE | cut -d ' ' -f 3)"
    SENDER="$(echo $MESSAGE | cut -d ' ' -f 5- | cut -d ';' -f 1)"
    TITLE="$(echo $MESSAGE | cut -d ' ' -f 5- | cut -d ';' -f 2 | sed -e 's/^ //g')"
    notify-send "$SENDER" "$TITLE"
done <<< "$NEW_MAIL"

notmuch tag -new tag:new
