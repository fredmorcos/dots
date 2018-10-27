#!/bin/bash

sudo cryptsetup luksOpen /dev/sdb Multimedia
sudo mount -o rw,noatime,norelatime,users,noauto /dev/mapper/Multimedia /Multimedia

