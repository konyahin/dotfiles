#!/usr/bin/env sh

set -e

REPO=$1

if [ -z $REPO ]
then
    echo "You should specify repository name"
    exit 1
fi

mkdir -p /git/$REPO
cd /git/$REPO
git init --bare

echo "Anton Konyahin" > owner
echo "git://git.konyahin.xyz/$REPO" > url
${EDITOR:-vi} description

chown -R git:git /git/$REPO
