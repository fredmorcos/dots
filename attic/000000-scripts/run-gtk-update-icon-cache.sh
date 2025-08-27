#!/bin/sh

for i in echo /usr/share/icons/*; do
    sudo gtk-update-icon-cache -f $i
done
