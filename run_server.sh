#!/bin/bash

set -e

photodir="$1"
connstr="$2"
test="$3"

wd=$(dirname $(realpath $0))
cd "$wd"

function on_exit {
    kill $(cat /tmp/nginx.pid)
    rm /tmp/nginx.{error,access}.log
    [ -n test ] && rm -rf "$photodir"
}

mkdir -p "$photodir"
cat > env.vars <<EOF
set \$photodir $photodir;
set \$errdir $wd;
EOF

trap on_exit EXIT
nginx -c "$wd/nginx.conf" && echo Nginx started

cabal run migrate "$connstr" && echo Shoe table migrated
cabal run server 8080 "$photodir" "$connstr" 10
