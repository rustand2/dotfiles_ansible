#!/bin/bash

for FILE in $(find roles -wholename '*/files/*' -type f); do
    LOCAL_FILE=$(echo $FILE | sed -e 's/^.*files//g' | sed -e "s|HOME|home/$USER|g")
    cp $LOCAL_FILE $FILE
done

mkdir -p temp
for TEMPLATE in $(find roles -wholename '*/templates/*' -type f); do
    LOCAL_FILE=$(echo -n "$TEMPLATE" | sed -e 's/^.*templates//g' | sed -e "s|HOME|home/$USER|g" | sed -e "s/\.j2$//g")
    FILE=$(basename "$LOCAL_FILE")
    ansible all -i "localhost," -m template -a "src=$TEMPLATE dest=temp/$FILE" --connection=local -e @vars.yml &> /dev/null
    diff -q $LOCAL_FILE temp/$FILE
    rm temp/$FILE
done
