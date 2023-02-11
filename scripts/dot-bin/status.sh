#!/usr/bin/env sh

set -e

while true; do
    LAYOUT=$(setxkbmap -query | grep layout | awk '{print $2}')
    CHARGE=$(apm -l)
    DATE=$(date '+ %H:%M %d-%m-%Y')

	xsetroot -name "🔋$CHARGE% $LAYOUT $DATE"
    sleep 0.2
done
