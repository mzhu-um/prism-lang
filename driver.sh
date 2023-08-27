#!/bin/bash

prism=~/prism/prism/bin/prism

src=$1
src_base=$(basename $src)
model_name="${src_base%.*}"
prism_name="${model_name}.prism"
spec_name="${model_name}.props"

echo $model_name
echo $prism_name

if [[ (-s $prism_name) && ($prism_name -ot $src) ]]; then
    rm $prism_name;
fi

if [[ ! (-s $prism_name) ]]; then
    echo "$prism_name is not present, generate a new one!"
    racket $src_base > $prism_name
else
    echo "$prism_name is present, skip generation!"
fi

if [[ -s $prism_name ]]; then
    if [ -s $spec_name ]; then
        $prism $prism_name $spec_name;
    else
        $prism $prism_name;
    fi
else
    echo "Racket Preprocessing Failed"
fi
