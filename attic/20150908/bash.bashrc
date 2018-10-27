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

PS1='[\j][$?] \W \$ '

shopt -s autocd
shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist 
shopt -s dirspell
shopt -s histappend

# OPAM configuration
# . /home/fred/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

alias y=yaourt
alias em='emacs'
alias grep='grep --color'
alias ls='ls --color'
alias ll='ls -lh --group-directories-first'
alias la='ll -A'
alias mv='mv -i'
alias cp='cp -i'

alias cdrpi='cd /home/fred/Workspace/rpi'
alias cdpet='cd /home/fred/Workspace/Projects/pet'

alias neuron_reboot='ssh neuron sudo systemctl reboot'
alias neuron_poweroff='ssh neuron sudo systemctl poweroff'
alias neuron_kodi_restart='ssh neuron sudo systemctl restart kodi'
alias neuron_kodi_stop='ssh neuron sudo systemctl stop kodi udisks polkit upower avahi-dnsconfd avahi-daemon.service'
alias neuron_rsyncd_start='ssh neuron sudo systemctl start rsyncd'
alias neuron_rsyncd_stop='ssh neuron sudo systemctl stop rsyncd'

alias work_push='rsync -rhu --delete --progress --preallocate ~/Workspace/projects/mediacenter/ neuron::Fred/work/mediacenter/ && ssh neuron "cd ~/work/mediacenter && make clean && make run"'

alias download_subtitle='subdl -i --output="%m.%L.%S"'

export EDITOR=emacs
export HISTCONTROL=ignoreboth
# export MANPAGER=most

cpupowersave () {
	sudo cpupower -c all frequency-set -g powersave
}

cpupowerperf () {
	sudo cpupower -c all frequency-set -g performance
}

cpupowerdyn () {
	sudo cpupower -c all frequency-set -g ondemand
}

up () {
    yaourt -Syu
}
