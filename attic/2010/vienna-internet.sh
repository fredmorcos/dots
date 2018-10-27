#!/bin/bash

killall dhcpcd
ifconfig eth1 down
ifconfig eth1 hw ether 00:1e:8c:52:9e:e3
ifconfig eth1 up
dhcpcd eth1

