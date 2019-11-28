# User-related
umask 077
export PASSFILE=/home/fred/Documents/Important/Passwords/Passwords.txt

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
PATH="$HOME/Oracle/opt/node13/bin:$PATH"
PATH="$HOME/Oracle/opt/zoom:$PATH"
LD_LIBRARY_PATH="$HOME/Oracle/opt/zoom:$LD_LIBRARY_PATH"

# Path
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/Documents/Workspace/bin:$PATH"

# Rust
RUSTC_WRAPPER=sccache
RUSTFLAGS="-C link-arg=-fuse-ld=lld"

# Editors
EDITOR='emacs -nw'
GIT_EDITOR=$EDITOR

export PATH EDITOR GIT_EDITOR RUSTC_WRAPPER
