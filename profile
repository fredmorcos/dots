# User-related
umask 077

# Applications
export MPV_HOME="$HOME/.config/mpv"

# Path
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"

# Rust
# RUSTC_WRAPPER=sccache
# RUSTFLAGS="-C opt-level=2 -C link-arg=-fuse-ld=lld -C target-cpu=native"

# export RUSTC_WRAPPER RUSTFLAGS

# Editors
EDITOR='emacs -nw'
GIT_EDITOR=$EDITOR

export PATH LD_LIBRARY_PATH EDITOR GIT_EDITOR
