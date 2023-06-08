#!/usr/bin/env sh

while true; do
    LAYOUT=$(setxkbmap -query | grep layout | awk '{print $2}')
    CHARGE=$(apm -l)
    DATE=$(date '+ %H:%M %d-%m-%Y')

    SONG=$(mpc current)
    if [ -n "$SONG" ]; then
	SONG="🎧 $SONG "
    fi

    xsetroot -name "$SONG🔋$CHARGE% $LAYOUT $DATE"
    sleep 0.2
done
