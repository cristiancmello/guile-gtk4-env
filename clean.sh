#!/bin/bash 

podman system prune -af
podman volume prune -f
