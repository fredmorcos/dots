#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
[[ -f /etc/profile.d/vte.sh ]] && . /etc/profile.d/vte.sh

PS1='\[\e[7;34m\]\w\[\e[0m\] >  '
HISTCONTROL=ignoredups
HISTSIZE=10000

export PS1 HISTCONTROL HISTSIZE

shopt -s checkwinsize

alias grep='grep --color'
alias ls='exa'
alias ll='ls -lg --group-directories-first'
alias la='ll -a'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias dired='emacs -nw .'
alias ip='ip -c'
alias fd='fd -iIH'
alias bat='bat --theme GitHub'
alias cat='bat'
alias up='yay --devel -Syu'

alias neuron_systemctl='ssh root@neuron systemctl'
alias neuron_reboot='neuron_systemctl reboot'
alias neuron_poweroff='neuron_systemctl poweroff'
alias neuron_kodi_restart='neuron_systemctl restart kodi'
alias neuron_kodi_stop='neuron_systemctl stop kodi'
alias neuron_update='ssh root@neuron pacman -Syu'

alias vpn='/usr/bin/openpyn de -t 5 -f -m 40'
alias vpn_p2p='vpn --p2p'
alias vpn_kill='sudo /usr/bin/openpyn -x'

alias yt480='youtube-dl -f "[height<=480]"'
alias yt720='youtube-dl -f "[height<=720]"'
alias yt1080='youtube-dl -f "[height<=1080]"'

alias largest="find . -type f -printf '%s %p\n' | sort -nr | head -20"

DLSUB_CMD="subdl -i --output={m}.{L}.{S}"
if [ -f "$PASSFILE" ]; then
  DLSUB_UN=$(passuser OpenSubtitles)
  DLSUB_PW=$(passpass OpenSubtitles)
  DLSUB_CMD="$DLSUB_CMD --username $DLSUB_UN --password $DLSUB_PW"
fi
# shellcheck disable=SC2139
alias dlsub="$DLSUB_CMD"
unset DLSUB_CMD

dirdlsub() {
  for i in *; do
    echo ">>> $i";
    dlsub "$i";
    echo "-----------------------------------------";
  done
}

cpuperf() {
  sudo cpupower -c all frequency-set -g performance
  sudo cpupower -c all set --perf-bias 0
}

cpusave() {
  sudo cpupower -c all frequency-set -g powersave
  sudo cpupower -c all set --perf-bias 15
}

gitall() {
  for i in *; do
    if [ -e "$i/.git" ] || [ -d "$i/.git" ]; then
      cd "$i" || exit
      echo ">>> git $*: $i"
      git "$@"
      cd .. || exit
    fi
  done
}

alias gitallstatus='gitall status -s'
alias gitallpp='gitall pull -q && gitall push -q'
