#!/bin/bash

podman build -t guile-gtk-dev .
distrobox stop guile-gtk-dev -Y && distrobox rm guile-gtk-dev -Y
distrobox create -n guile-gtk-dev \
    --nvidia \
    --image localhost/guile-gtk-dev:latest \
    --home /home/cristian/Distroboxes/guile-gtk4-dev
