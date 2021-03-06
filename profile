# User-related
umask 077

OS_NAME=$(head -1 ~/Documents/Important/Passwords/_os.txt)
OS_PASS=$(tail -1 ~/Documents/Important/Passwords/_os.txt)

export OS_NAME OS_PASS

# Applications
export MPV_HOME="$HOME/.config/mpv"

# AMD
# export AMD_DEBUG="nongg,nodma"
# export RADV_PERFTEST="aco"

# Qt
# export QT_AUTO_SCREEN_SCALE_FACTOR=1
# export QT_SCALE_FACTOR=2

# Gtk3
# export GDK_SCALE=2       # 2x scaling
# export GDK_DPI_SCALE=0.5 # Undo scaling of text

# Java-specific
PATH="$HOME/Oracle/mx:$PATH"
PATH="$HOME/Oracle/opt/oraclejdk-jvmci/bin:$PATH"
PATH="$HOME/Oracle/opt/oraclejdk-jvmci/jre/bin:$PATH"
# PATH="$HOME/Oracle/opt/jdk-15/bin:$PATH"
# PATH="$HOME/Oracle/opt/jdk-15/jre/bin:$PATH"
PATH="$HOME/Oracle/opt/eclipse:$PATH"

export MX_PYTHON_VERSION=3

# Other
PATH="$HOME/Oracle/opt/zoom:$PATH"
PATH="$HOME/Oracle/opt/idea-IU-193.5233.102/bin:$PATH"

# Path
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/Documents/Workspace:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"

LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"

# Rust
RUSTC_WRAPPER=sccache
RUSTFLAGS="-C opt-level=2 -C link-arg=-fuse-ld=lld -C target-cpu=native"

export RUSTC_WRAPPER RUSTFLAGS

# Editors
EDITOR='emacs -nw'
GIT_EDITOR=$EDITOR

export PATH LD_LIBRARY_PATH EDITOR GIT_EDITOR
