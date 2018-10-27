export PATH=$PATH:/sbin:/usr/sbin:/home/fred/bin
export EDITOR=vim

export HISTCONTROL=ignoreboth

export GEGL_USE_OPENCL=yes
export MOZ_DISABLE_PANGO=1

export VDPAU_NVIDIA_NO_OVERLAY=1

alias ls='ls --color=auto'
alias ll='ls -lh --color=auto --group-directories-first'
alias grep='grep --color=auto'

alias cpugov="cpupower -c all frequency-info | grep -i \"current cpu\""
alias skype="LIBV4LCONTROL_FLAGS=3 LD_PRELOAD=/usr/lib32/libv4l/v4l1compat.so skype"
