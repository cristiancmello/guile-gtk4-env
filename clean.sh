#!/bin/bash 

distrobox stop guile-gtk-dev -Y && distrobox rm guile-gtk-dev -Y
podman system prune -af
podman volume prune -f
