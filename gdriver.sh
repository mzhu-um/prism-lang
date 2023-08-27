#!/bin/bash

PRISM_BIN=~/git/prism/prism/bin

src=$1
src_base=$(basename $src)
model_name="${src_base%.*}"
prism_name="${model_name}.prism"
spec_name="${model_name}.props"

echo $model_name
echo $prism_name

if [ -s $prism_name ]; then
    rm $prism_name;
fi

racket $src_base > $prism_name

if [ -s $prism_name ]; then
    if [ -s $spec_name ]; then
        $PRISM_BIN/xprism $prism_name $spec_name;
    else
        $PRISM_BIN/xprism $prism_name;
    fi
else
    echo "Racket Preprocessing Failed"
fi
