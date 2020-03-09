#!/usr/bin/env bash
# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../background"
cd $DIR
bgs=(*.png)

LOG_FOLDER="$HOME/_myScreenLock"
mkdir -p "$LOG_FOLDER"
LOG_FILE="$LOG_FOLDER/$(date '+%Y-%V')-myScreenLock.sh.log"

getRandomBG() {
    rand=$[$RANDOM % ${#bgs[@]}]
    img="${bgs[$rand]}"

    if [ "$1" ]; then
        if [ -d "$DIR/$1" ]; then
            echo "$DIR/$1/${bgs[$rand]}"
            return
        fi
    fi
    echo "$DIR/${bgs[$rand]}"
}

setRandomWithFeh() {
    [ -z "$DISPLAY" ] && \
        export DISPLAY=:0
    feh --bg-scale "$(getRandomBG)"
}

getScreenHeigth() {
    xrandr --current |
        grep '*' |
        uniq |
        awk '{print $1}' |
        cut -d 'x' -f2
}

lockWithRandom() {
    echo "lock at: $(date)" | tee -a "$LOG_FILE"
    export XSECURELOCK_SAVER=saver_mpv
    export XSECURELOCK_LIST_VIDEOS_COMMAND="feh --zoom=fill -F '$(getRandomBG $(getScreenHeigth))'"
    xsecurelock
    echo "unlock at: $(date)" | tee -a "$LOG_FILE"
}

case $1 in
    --set) setRandomWithFeh ;;
    --lock) lockWithRandom ;;
    *) getRandomBG ;;
esac
