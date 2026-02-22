FROM fedora:43

ARG GGOLF_REPO=https://git.savannah.gnu.org/git/g-golf.git
ARG GGOLF_REF=v0.8.3

RUN dnf update -y && \
    dnf install -y glibc-langpack-en glibc-locale-source && \
    localedef -i en_US -f UTF-8 en_US.UTF-8 && \
    dnf install -y --setopt=install_weak_deps=False --setopt=tsflags=nodocs \
        pciutils wget git \
        gcc gcc-c++ make automake autoconf libtool pkgconfig texinfo \
        guile30-3.0.9-5.fc43 guile30-devel-3.0.9-5.fc43 \
        glib2-devel-2.86.4-1.fc43 gettext-devel \
        gobject-introspection-1.84.0-3.fc43 gobject-introspection-devel-1.84.0-3.fc43 \
        gtk3-devel-3.24.51-2.fc43 \
        gtk4-devel-4.20.2-1.fc43 \
        libadwaita-1.8.4-1.fc43 libadwaita-devel-1.8.4-1.fc43 \
        vulkan-loader mesa-vulkan-drivers vulkan-tools \
        mesa-libGLES libwayland-client libwayland-cursor libwayland-egl libxkbcommon-devel \
    && dnf clean all \
    && rm -rf /var/cache/dnf

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

RUN ln -sf /usr/bin/guile3.0 /usr/bin/guile && \
    ln -sf /usr/bin/guild3.0  /usr/bin/guild

RUN git clone --branch "${GGOLF_REF}" "${GGOLF_REPO}" /tmp/g-golf && \
    cd /tmp/g-golf && \
    ./autogen.sh && \
    ./configure \
        --prefix=/usr \
        --libdir=/usr/lib64 \
        --with-guile-site=yes \
        GUILE=/usr/bin/guile3.0 \
        GUILD=/usr/bin/guild3.0 && \
    make -j"$(nproc)" && \
    make install && \
    cd / && rm -rf /tmp/g-golf && \
    ldconfig

ENV GUILE_LOAD_PATH="/usr/share/guile/site/3.0"
ENV GUILE_EXTENSIONS_PATH="/usr/lib64/guile/3.0/extensions"
ENV GDK_BACKEND=wayland
ENV GSK_RENDERER=vulkan
ENV GDK_DEBUG=vulkan
ENV QT_QPA_PLATFORM=wayland
ENV XDG_SESSION_TYPE=wayland

RUN useradd -m -s /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/dev

USER dev
WORKDIR /home/dev

CMD ["/bin/bash"]