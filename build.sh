#!/bin/bash

LOG_FILE="build-$(date +%Y%m%d-%H%M%S).log"

exec > >(tee -a "build-logs/$LOG_FILE") 2>&1

echo "=== Build iniciado em $(date) ==="
echo "=== Log: $LOG_FILE ==="

# podman build --no-cache -t guile-gtk-dev .
podman build -t guile-gtk-dev .
distrobox stop guile-gtk-dev -Y && distrobox rm guile-gtk-dev -Y
distrobox create -n guile-gtk-dev \
    --nvidia \
    --image localhost/guile-gtk-dev:latest \
    --home /home/cristian/Distroboxes/guile-gtk4-dev

echo "=== Build finalizado em $(date) ==="