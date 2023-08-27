#!/bin/bash

# existsp=$(raco pkg show  | grep scribble-bettergrammar | awk '{ print $1 }')

# if [[ "$existsp" != 'scribble-bettergrammar' ]]; then
#     raco pkg install scribble-bettergrammar;
# fi

raco setup --pkgs prism-lang
