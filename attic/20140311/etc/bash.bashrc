#
# /etc/bash.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '
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

PS1='[\j][$?] \W \$ '

shopt -s autocd
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist 
shopt -s dirspell
shopt -s histappend

export PATH=/home/fred/.cabal/bin:/home/fred/System/bin:$PATH
export LD_LIBRARY_PATH=/home/fred/.cabal/lib:$LD_LIBRARY_PATH

alias y=yaourt
alias em='emacs'
alias grep='grep --color'
alias ls='ls --color'
alias ll='ls -lh --group-directories-first'
alias la='ll -A'
alias mv='mv -i'
alias cp='cp -i'

alias cdrpi='cd /home/fred/Workspace/Projects/rpi'

export EDITOR=emacs
export HISTCONTROL=ignoreboth

cpupowersave () {
	sudo cpupower -c all frequency-set -g powersave
}

cpupowerperf () {
	sudo cpupower -c all frequency-set -g performance
}

cpupowerdyn () {
	sudo cpupower -c all frequency-set -g ondemand
}

