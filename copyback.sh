#!/bin/bash
FILES=$(find roles -wholename '*/files/*' -type f)

for F in $FILES; do
    LOCAL_FILE=$(echo $F | sed -e 's/^.*files//g' | sed -e "s|HOME|home/$USER|g")
    cp $LOCAL_FILE $F
done
