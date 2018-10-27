#!/usr/bin/env bash

set -o errexit

cd ~

nvidia-settings --load-config-only
nvidia-settings -a InitialPixmapPlacement=2

echo "NVIDIA Config Done" > ~/System/bin/nvidialog
