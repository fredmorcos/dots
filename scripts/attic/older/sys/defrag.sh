#!/usr/bin/sh

cmd="btrfs filesystem defragment -r -f -c -v"

for n in /home/fred/Documents /home/fred/Firefox /opt/Steam /home/fred/.local/share/Steam; do
    $cmd $n
done
