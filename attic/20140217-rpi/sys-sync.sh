#!/bin/sh

RSYNC_CMD="rsync -rhu --delete --progress --preallocate --dry-run"

if ! which rsync &>/dev/null; then
    echo "Cannot find rsync" 1>&2
    exit 1
fi

echo "home directory = $HOME"
echo "rsync cmd = $RSYNC_CMD"
echo "----"

if [[ -d "$HOME/Workspace/Projects/rpi/config" ]]; then
    dir="$HOME/Workspace/Projects/rpi/config"
    echo "Going to sync locally to $dir"

    if ! ping -c 1 neuron &>/dev/null; then
	echo "Neuron is not online" 1>&2
	echo "Cannot sync system files" 1>&2
	exit 1
    else
	echo "Neuron is online"
    fi
else
    echo "$HOME/Workspace/Projects/rpi/config does not exist" 1>&2
    echo "Cannot sync system files" 1>&2
    exit 1
fi

echo "----"

for i in init etc/fstab etc/bash.bashrc etc/pacman.conf etc/systemd/journald.conf etc/systemd/system/hdparm.service boot/config.txt boot/cmdline.txt media/External/System/bin/fixperms.sh; do
    echo "Sync neuron:/$i  -->  $dir/$i"
    if ! $RSYNC_CMD "neuron:/$i" "$dir/$i"; then
	echo "Could not sync $i" 1>&2
	exit 1
    fi
done
