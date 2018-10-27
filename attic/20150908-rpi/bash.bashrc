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
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

    ;;
  screen)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

PS1='NEURON [\j][$?] \W \$ '

shopt -s autocd
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist
shopt -s dirspell
shopt -s histappend

export PATH=/media/External/System/bin:$PATH

alias y=yaourt
alias em='emacs'
alias grep='grep --color'
alias ls='ls --color'
alias ll='ls -lh --group-directories-first'
alias la='ll -A'
alias mv='mv -i'
alias cp='cp -i'

alias cdmedia='cd /media/External/Common/'

alias play='omxplayer -p --nativedeinterlace -w -b --align center'

alias download_subtitle='subdl -i --output="%m.%L.%S"'

export EDITOR=emacs
export HISTCONTROL=ignoreboth

