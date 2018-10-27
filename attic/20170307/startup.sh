#!/bin/sh

set -o posix
set -o errexit
set -o nounset
set -o pipefail

xrdb -merge /home/fred/.Xresources
i3lock -c 000000 &

# compton &
/usr/bin/xset -dpms &
setxkbmap -option 'ctrl:nocaps' &
xbindkeys &
redshift-gtk &
firefox-beta &

# nvdock &
# steam-native -silent &

xfce4-terminal &
emacs &
