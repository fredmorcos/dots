#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# PS1='[\u@\h \W]\$ '
PS2='> '
PS3='> '
PS4='+ '

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
                                                        
    ;;
  screen)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
    ;;
esac

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

PS1='[\j jobs][$?] \W \$ '

shopt -s autocd
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist 
shopt -s dirspell
shopt -s histappend

alias ls='ls --color=auto'
alias ll='ls -lh --group-directories-first'
alias la='ll -A'
alias mv='mv -i'
alias grep='grep --color=auto'
alias em='emacs'

export EDITOR=emacs
export HISTCONTROL=ignoreboth

export PATH=$PATH:/usr/bin/vendor_perl

if [[ "$(hostname)" == "archvm" ]]; then
    export http_proxy="http://proxy:911/"
    export https_proxy="http://proxy:911/"
    export HTTP_PROXY="$http_proxy"
    export HTTPS_PROXY="$https_proxy"
fi

cpupowersave () {
    sudo cpupower -c all frequency-set -g powersave
}

cpupowerperf () {
    sudo cpupower -c all frequency-set -g performance
}

cpupowerdyn () {
    sudo cpupower -c all frequency-set -g ondemand
}

