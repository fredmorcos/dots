# Graphics
XFWM4_USE_PRESENT=1
# LIBVA_DRIVER_NAME=iHD
# MESA_LOADER_DRIVER_OVERRIDE=iris

export XFWM4_USE_PRESENT # LIBVA_DRIVER_NAME MESA_LOADER_DRIVER_OVERRIDE

# Qt
# export QT_AUTO_SCREEN_SCALE_FACTOR=1
# export QT_SCALE_FACTOR=2

# Gtk3
export GDK_SCALE=2       # 2x scaling
export GDK_DPI_SCALE=0.5 # Undo scaling of text

# Java-specific
PATH="$HOME/Oracle/oraclejdk/bin:$PATH"
PATH="$HOME/Oracle/oraclejdk/jre/bin:$PATH"
PATH="$HOME/Oracle/eclipse:$PATH"
PATH="$HOME/Oracle/mx:$PATH"

# Path
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/Documents/Workspace/bin:$PATH"

# Rust
RUSTC_WRAPPER=sccache

# Editors
EDITOR='emacs -nw'
GIT_EDITOR=vim

export PATH EDITOR GIT_EDITOR RUSTC_WRAPPER
