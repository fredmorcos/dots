#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

source <(kitty + complete setup bash)

# PS1='[\u@\h \W]\$ '
PS1='\[\e[7;34m\]\w\[\e[0m\] >  '

PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/Workspace/dots/scripts:$PATH"
PATH="$HOME/Documents/Workspace/bin:$PATH"

# bind "set completion-ignore-case on"
# bind "set completion-map-case on"
bind "set show-all-if-ambiguous on"
bind "set mark-symlinked-directories on"
bind "set match-hidden-files on"
bind "set skip-completed-text on"
bind "set colored-stats on"

shopt -s histappend
shopt -s cmdhist

HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history"

shopt -s autocd
shopt -s dirspell
shopt -s cdspell

alias -- -='cd -'

alias grep='grep --color'
alias ls='exa'
alias ll='ls -lghH --group-directories-first --git -@'
alias la='ll -a'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias dired='emacs -nw .'
alias ip='ip -c'
alias fd='fd -iIH'
alias bat='bat --theme GitHub'
alias cat='bat'
alias fzf='fzf -e --color=light'
alias diff='diff-so-fancy'

alias ssh='kitty +kitten ssh'

alias neuron_systemctl='ssh neuron sudo systemctl'
alias neuron_reboot='neuron_systemctl reboot'
alias neuron_poweroff='neuron_systemctl poweroff'
alias neuron_kodi_restart='neuron_systemctl restart kodi'
alias neuron_kodi_stop='neuron_systemctl stop kodi'
alias neuron_update='ssh neuron sudo pacman -Syu'

alias synapse_systemctl='ssh synapse sudo systemctl'
alias synapse_reboot='synapse_systemctl reboot'
alias synapse_poweroff='synapse_systemctl poweroff'
alias synapse_update='ssh synapse sudo pacman -Syu'

alias dlsub='subdl -i --output={m}.{L}.{S}'
alias yt720='youtube-dl -f "[height<=720]"'
alias largest="find . -type f -printf '%s %p\n' | sort -nr | head -20"

dirdlsub() {
	for i in *; do
    echo "$i";
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
