# User-related
umask 077

# XDG
XDG_DATA_HOME="$HOME/.local/share"
XDG_CONFIG_HOME="$HOME/.config"
XDG_STATE_HOME="$HOME/.local/state"
XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME XDG_CONFIG_HOME XDG_STATE_HOME XDG_CACHE_HOME

# Bash
HISTFILE="$XDG_STATE_HOME/bash_history"
export HISTFILE

# GnuPG
GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GNUPGHOME

# Screen
SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"
export SCREENRC

# Less
LESSHISTFILE="$XDG_STATE_HOME/less_history"
export LESSHISTFILE

# Applications
MPV_HOME="$HOME/.config/mpv"
export MPV_HOME

# Binary and library paths
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/.local/share/rust/cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="/usr/lib/ccache/bin/:$PATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
export PATH LD_LIBRARY_PATH

# Rust
# RUSTC_WRAPPER=sccache
# RUSTFLAGS="-C opt-level=2 -C link-arg=-fuse-ld=lld -C target-cpu=native"
# export RUSTC_WRAPPER RUSTFLAGS

# Rustup and Cargo
RUSTUP_HOME="$XDG_DATA_HOME/rust/rustup"
CARGO_HOME="$XDG_DATA_HOME/rust/cargo"
export RUSTUP_HOME CARGO_HOME

# GnuPG
GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GNUPGHOME

# Inputrc
INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export INPUTRC

# Editors
EDITOR=emacs
GIT_EDITOR=$EDITOR
export EDITOR GIT_EDITOR

# On-demand debugging symbols
DEBUGINFOD_URLS="https://debuginfod.archlinux.org/ https://debuginfod.elfutils.org/"
export DEBUGINFOD_URLS
