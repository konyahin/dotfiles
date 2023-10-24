#!/usr/bin/env sh

set -e

MALLOC_OPTIONS=D ktrace -tu "$1"

echo "\nLEAKS:"
kdump -u malloc |
    awk '/addr2line/{ system("addr2line " $6 " " $7 " " $8) }'
