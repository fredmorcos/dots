#!/bin/bash

sudo modprobe kqemu
sudo mknod /dev/kqemu c 250 0
sudo chmod 666 /dev/kqemu 
