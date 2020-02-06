# User-related
umask 077
export PASSFILE="$HOME/Documents/Important/Passwords/Passwords.txt"

# export AMD_DEBUG="nongg nodma"
export RADV_PERFTEST="aco"

# Qt
# export QT_AUTO_SCREEN_SCALE_FACTOR=1
# export QT_SCALE_FACTOR=2

# Gtk3
# export GDK_SCALE=2       # 2x scaling
# export GDK_DPI_SCALE=0.5 # Undo scaling of text

# Java-specific
PATH="$HOME/Oracle/mx:$PATH"
PATH="$HOME/Oracle/opt/oraclejdk/bin:$PATH"
PATH="$HOME/Oracle/opt/oraclejdk/jre/bin:$PATH"
PATH="$HOME/Oracle/opt/eclipse:$PATH"

export MX_PYTHON_VERSION=3

# Other
PATH="$HOME/Oracle/opt/zoom:$PATH"
LD_LIBRARY_PATH="$HOME/Oracle/opt/zoom:$LD_LIBRARY_PATH"
PATH="$HOME/Oracle/opt/idea-IU-193.5233.102/bin:$PATH"

# Path
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/Documents/Workspace:$PATH"
PATH="$HOME/.cargo/bin:$PATH"

# Rust
RUSTC_WRAPPER=sccache
RUSTFLAGS="-C link-arg=-fuse-ld=lld"

export RUSTC_WRAPPER RUSTFLAGS

# Editors
EDITOR='emacs -nw'
GIT_EDITOR=$EDITOR

export PATH EDITOR GIT_EDITOR
