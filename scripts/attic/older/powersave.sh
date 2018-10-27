#!/bin/bash

iw dev wlan0 set power_save off

echo 'min_power' > '/sys/class/scsi_host/host0/link_power_management_policy'
echo '1500' > '/proc/sys/vm/dirty_writeback_centisecs'
echo '1' > '/sys/module/snd_hda_intel/parameters/power_save'
echo 'auto' > '/sys/bus/usb/devices/1-1.1/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:05:00.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1a.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:00.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:01.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:02.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:16.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:04:00.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:01:00.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1b.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:03:00.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1d.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.5/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.3/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.0/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.1/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.2/power/control'
echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.3/power/control'
