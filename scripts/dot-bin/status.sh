#!/usr/bin/env sh

clean_task () {
    echo "$1" | sed -E 's/^[0-9]+-[0-9]+-[0-9]+ //' |
        sed -E 's/\+active//' |
        sed -E 's/ \@[[:alpha:]]+//'
}

while true; do
    LAYOUT=$(setxkbmap -query | grep layout | awk '{print $2}')
    CHARGE=$(apm -l)
    DATE=$(date '+ %H:%M %d-%m-%Y')

    #SONG=$(mpc current)
    [ -n "$SONG" ] && SONG="🎧 $SONG "

    TASK=$(grep "+active" /home/anton/data/notes/todo.txt)
    [ -n "$TASK" ] && TASK="🔵 $(clean_task "$TASK")"

    xsetroot -name "$TASK $SONG🔋$CHARGE% $LAYOUT $DATE"
    sleep 0.2
done
