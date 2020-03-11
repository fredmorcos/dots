#!/bin/bash

sudo hdparm -S 0 /dev/sdc	> /dev/null
# sudo hdparm -S 0 /dev/sdd	> /dev/null
sudo hdparm -B 255 /dev/sdc	> /dev/null
# sudo hdparm -B 255 /dev/sdd > /dev/null

RSYNC_COMMAND="rsync --progress --recursive --verbose --size-only --delete --ignore-times --human-readable --modify-window=3600"

if [ -d "/media/fred-portable/" ]; then
	echo "Syncing personal to fred-portable..."
	$RSYNC_COMMAND /personal/ /media/fred-portable/
fi

if [ -d "/media/external/" ]; then
	if [ -d "/media/multimedia/" ]; then
		echo "Syncing multimedia to external..."
		$RSYNC_COMMAND /media/multimedia/ /media/external/
	fi
fi

