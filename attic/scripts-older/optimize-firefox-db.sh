#!/bin/sh

ffpid=$(pidof firefox)

if [[ "$ffpid" -eq "" ]]; then
    find ~/.mozilla/firefox/*.default -name \*.sqlite -exec sqlite3 {} vacuum \; -exec sqlite3 {} reindex \;
fi
