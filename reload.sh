#!/bin/bash

cmd="$1"
shift

inotifywait --quiet --recursive --event modify ./ --exclude .git/ && "$cmd" "$@" 


./reload.sh "$cmd" "$@"

