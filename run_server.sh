#!/bin/bash

set -e

photodir="$1"
connstr="$2"

wd=$(dirname $(realpath $0))
cd "$wd"

mkdir -p "$photodir"
echo set '$photodir' $photodir';' > env.vars
trap "killall nginx" EXIT
nginx -c "$wd/nginx.conf" && echo Nginx started

cabal run migrate "$connstr" && echo Shoe table migrated
cabal run server 8080 "$photodir" "$connstr" 10
