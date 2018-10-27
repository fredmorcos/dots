# Gtk
export NO_AT_BRIDGE=1

# Steam
export STEAM_FRAME_FORCE_CLOSE=1

# Vulkan, OpenGL, Clutter
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/nvidia_icd.json

# Firefox
# export MOZ_USE_OMTC=1
# export MOZ_USE_XINPUT2=1

# Timezone
export TZ=:/etc/localtime

# General
H=""

if [ "$(hostname)" == "axon" ]; then
  H="/home/fred"
else
  H="/home/fnm"
fi

WS=$H/Workspace

# Scripts and local programs
# PATH=$H/opt/firefox:$PATH
# PATH=$H/opt/thunderbird:$PATH
PATH=$H/Documents/Workspace/bin:$PATH
PATH=$WS/bin:$PATH
PATH=$WS/dotfiles/scripts:$PATH

# Libraries
LD_LIBRARY_PATH=/usr/lib
LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Rust
# export RUSTC_WRAPPER=sccache
export RUSTFLAGS="-C target-cpu=native -C target-feature=avx2"
PATH=$H/.cargo/bin:$PATH
# CARGO_TARGET_DIR=$H/.cargo/project_targets
RUST_PATH="$(rustc --print sysroot)"
PATH=$RUST_PATH/bin:$PATH
LD_LIBRARY_PATH=$RUST_PATH/lib:$LD_LIBRARY_PATH
RUST_SRC_PATH="$RUST_PATH/lib/rustlib/src/rust/src"
RLS_ROOT="$RUST_PATH/bin"

# NodeJS
PATH=$H/.node_modules/node_modules/.bin:$PATH

export RUST_SRC_PATH RUST_PATH RLS_ROOT
export PATH LD_LIBRARY_PATH
