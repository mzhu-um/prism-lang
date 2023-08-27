#!/bin/bash

prism=~/prism/prism/bin/prism

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
    $prism $prism_name $spec_name -exporttransdotstates ${model_name}-g.dot
    dot -Tpdf ${model_name}-g.dot -Gfontname="Source Sans Pro" -Efontname="Source Sans Pro" -Nfontname="Source Sans Pro" -o ${model_name}-g.pdf
    dot -Tsvg ${model_name}-g.dot -Gfontname="Source Sans Pro" -Efontname="Source Sans Pro" -Nfontname="Source Sans Pro" -o ${model_name}-g.svg
else
    echo "No PRISM Model Present; Abort!"
fi
