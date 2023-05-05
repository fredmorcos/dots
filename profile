# User-related
umask 077

# XDG
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# Applications
export HISTFILE="$XDG_STATE_HOME/bash/bash_history"      # Bash
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"       # Inputrc
export MPV_HOME="$HOME/.config/mpv"                      # MPV
export DVDCSS_CACHE="$XDG_DATA_HOME/dvdcss"              # DVD-CSS
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"         # GNU Parallel
export SCREENRC="$XDG_CONFIG_HOME/screen/screenrc"       # GNU Screen
export GNUPGHOME="$XDG_DATA_HOME/gnupg"                  # GNU PG
export LESSHISTFILE="$XDG_STATE_HOME/less/less_history"  # Less and most
export PAGER="less --use-color"                          # Less and most
export ANDROID_HOME="$XDG_DATA_HOME/android"             # Android tools
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"           # Docker
export PYTHONSTARTUP="/etc/python/pythonrc"              # Python
# shellcheck disable=SC2016
export GVIMINIT='let $MYGVIMRC="$XDG_CONFIG_HOME/vim/gvimrc" | source $MYGVIMRC'  # GVim
# shellcheck disable=SC2016
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'      # Vim

# Binary and library paths
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/.local/share/rust/cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="/usr/lib/ccache/bin/:$PATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
export PATH LD_LIBRARY_PATH

# Rust, rustup and Cargo
# export RUSTC_WRAPPER=sccache
# export RUSTFLAGS="-C opt-level=2 -C link-arg=-fuse-ld=lld -C target-cpu=native"
export RUSTUP_HOME="$XDG_DATA_HOME/rust/rustup"
export CARGO_HOME="$XDG_DATA_HOME/rust/cargo"

# Editors
export EDITOR=emacs
export GIT_EDITOR=$EDITOR

# On-demand debugging symbols
export DEBUGINFOD_URLS="https://debuginfod.archlinux.org/ https://debuginfod.elfutils.org/"
