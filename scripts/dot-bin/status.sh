#!/usr/bin/env sh

set -e

while true; do
	xsetroot -name "$(date '+ %A %H:%M %Y')"
	sleep 2
done
