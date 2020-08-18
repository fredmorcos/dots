#!/bin/sh

./configure \
    --prefix=/home/fred/.local \
    --sysconfdir=/etc \
    --libexecdir=/home/fred/.local/lib \
    --localstatedir=/home/fred/.local/var \
    --mandir=/home/fred/.local/share/man \
    --with-gameuser=:games \
    --enable-checking=no \
    --enable-link-time-optimization \
    --enable-largefile \
    --disable-gtk-deprecation-warnings \
    --disable-acl \
    --without-mailutils \
    --without-pop \
    --without-kerberos \
    --without-kerberos5 \
    --without-hesiod \
    --without-mail-unlink \
    --without-wide-int \
    --without-lcms2 \
    --without-libsystemd \
    --without-m17n-flt \
    --without-toolkit-scroll-bars \
    --without-xaw3d \
    --without-xim \
    --without-gpm \
    --without-dbus \
    --without-selinux \
    --without-xwidgets \
    --without-compress-install \
    --without-gconf \
    --without-gsettings \
    --without-sound \
    --without-imagemagick \
    --with-native-image-api \
    --with-jpeg \
    --with-tiff \
    --with-gif \
    --with-rsvg \
    --with-png \
    --with-xpm \
    --with-pdumper=yes \
    --with-sound=no \
    --with-harfbuzz \
    --with-cairo \
    --with-json \
    --with-xml2 \
    --with-xft \
    --with-libotf \
    --with-gnutls \
    --with-zlib \
    --with-modules \
    --with-threads \
    --with-file-notification=yes \
    --with-x-toolkit=gtk3 \
    --with-x \
    --with-libgmp \
    --with-nativecomp \
    CFLAGS="-O3 -mtune=native -march=native -pipe -fno-plt -fomit-frame-pointer"

make -j4 CFLAGS="-O3 -mtune=native -march=native -pipe -fno-plt -fomit-frame-pointer"
