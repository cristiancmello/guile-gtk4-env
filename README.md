# guile-gtk4-env

Fedora's container with GNU Guile + GTK4 + Vulkan + Wayland

```sh
podman system prune -af
podman build --no-cache -t guile-gtk-env .
distrobox stop guile-gtk-env -Y && distrobox rm guile-gtk-env -Y
distrobox create -n guile-gtk-env \
    --image localhost/guile-gtk-env:latest \
    --home /home/cristian/Distroboxes/guile-gtk4-dev
distrobox enter guile-gtk-env
```
