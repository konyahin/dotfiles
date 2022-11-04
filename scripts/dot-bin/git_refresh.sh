#!/usr/bin/env sh

set -e

rm -rf /var/www/htdocs/git/index.html

cd /git/
for repo in */ ; do
    mkdir -p /var/www/htdocs/git/"$repo"
    cd /var/www/htdocs/git/"$repo"
    stagit /git/"$repo"
done

cd /var/www/htdocs/git/
stagit-index /git/* >> index.html
