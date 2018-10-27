#!/bin/bash

function width {
    wmctrl -d | awk '/*/ {split($9, a, "x"); print a[1]}'
}

function height {
    wmctrl -d | awk '/*/ {split($9, a, "x"); print a[2]}'
}

# function screens {
#     xrandr 2>&1 | grep \* | wc -l
# }

# let s=$(screens)

let s=1
echo $s

if [[ "$s" == "2" ]]; then
    let w_=$(width)/2
else
    let w_=$(width)
fi

let w2=($w_)/2
let w3=($w_)/3
let w__=$w3*2
let h=$(height)/2

wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz

for arg in $@; do
    case $arg in
	"left" )
	    wmctrl -r :ACTIVE: -e 0,0,0,$w2,-1
	    wmctrl -r :ACTIVE: -b add,maximized_vert
	    ;;
	"right" )
	    wmctrl -r :ACTIVE: -e 0,$w2,0,$w2,-1
	    wmctrl -r :ACTIVE: -b add,maximized_vert
	    ;;
	"top" )
	    wmctrl -r :ACTIVE: -e 0,0,0,-1,$h
	    wmctrl -r :ACTIVE: -b add,maximized_horz
	    ;;
	"bottom" )
	    wmctrl -r :ACTIVE: -e 0,0,$h,-1,$h
	    wmctrl -r :ACTIVE: -b add,maximized_horz
	    ;;
	"topleft" )
	    wmctrl -r :ACTIVE: -e 0,0,0,$w2,$h
	    ;;
	"topright" )
	    wmctrl -r :ACTIVE: -e 0,$w2,0,$w2,$h
	    ;;
	"bottomleft" )
	    wmctrl -r :ACTIVE: -e 0,0,$h,$w2,$h
	    ;;
	"bottomright" )
	    wmctrl -r :ACTIVE: -e 0,$w2,$h,$w2,$h
	    ;;
	"left3" )
	    wmctrl -r :ACTIVE: -e 0,0,0,$w3,-1
	    wmctrl -r :ACTIVE: -b add,maximized_vert
	    ;;
	"middle3" )
	    wmctrl -r :ACTIVE: -e 0,$w3,0,$w3,-1
	    wmctrl -r :ACTIVE: -b add,maximized_vert
	    ;;
	"right3" )
	    wmctrl -r :ACTIVE: -e 0,$w__,0,$w3,-1
	    wmctrl -r :ACTIVE: -b add,maximized_vert
	    ;;
	"topleft3" )
	    wmctrl -r :ACTIVE: -e 0,0,0,$w3,$h
	    ;;
	"bottomleft3" )
	    wmctrl -r :ACTIVE: -e 0,0,$h,$w3,$h
	    ;;
	"topmiddle3" )
	    wmctrl -r :ACTIVE: -e 0,$w3,0,$w3,$h
	    ;;
	"bottommiddle3" )
	    wmctrl -r :ACTIVE: -e 0,$w3,$h,$w3,$h
	    ;;
	"topright3" )
	    wmctrl -r :ACTIVE: -e 0,$w__,0,$w3,$h
	    ;;
	"bottomright3" )
	    wmctrl -r :ACTIVE: -e 0,$w__,$h,$w3,$h
	    ;;
    esac
done

