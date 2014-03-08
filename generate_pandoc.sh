#!/bin/bash

wd=$(dirname $(realpath $0))
cd "$wd"

for name in src/Web/ZaloraTask/{Controller,View,Types} \
    tests/Web/ZaloraTask/ControllerSpec; do
    pandoc -r markdown+lhs -w html -s -c lhs.css "$name".lhs \
        > "$(basename "$name")".html
done
