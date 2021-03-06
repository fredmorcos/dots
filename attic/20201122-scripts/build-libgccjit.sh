#!/bin/sh

./configure --prefix="/home/fred/.local" --libexecdir="/home/fred/.local/lib" --with-system-zlib --with-linker-hash-style=gnu --enable-host-shared --enable-shared --enable-checking=release --enable-languages=jit --enable-linker-build-id --disable-multilib --disable-bootstrap --disable-libssp --disable-lto --disable-libquadmath --disable-liboffloadmic --disable-libada --disable-libsanitizer --disable-libquadmath-support --disable-libgomp --disable-libvtv

make -j3
make DESTDIR="/home/fred/.local" jit.install-common jit.install-info
