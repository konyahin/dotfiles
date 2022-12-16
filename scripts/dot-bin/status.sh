#!/usr/bin/env sh

set -e

while true; do
	xsetroot -name "🔋$(apm -l)% $(date '+ %H:%M %d-%m-%Y')"
	sleep 2
done
