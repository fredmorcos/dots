#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\[\e[7;34m\]\w\[\e[0m\] >  '
HISTCONTROL=ignoredups
HISTSIZE=1000

export PS1 HISTCONTROL HISTSIZE

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

data_mount() {
  sudo cryptsetup open /dev/disk/by-label/EncryptedData data
  mkdir -p ~/Data
  if [ "$1" == "check" ]; then
    sudo btrfs check -p /dev/mapper/data
  fi
  sudo mount -o defaults,rw,relatime,compress=zstd /dev/mapper/data ~/Data
}

data_unmount() {
  sudo umount ~/Data
  rmdir ~/Data
  sudo cryptsetup close data
}

floron_mount() {
  mkdir -p ~/Floron
  sshfs fred@floron:/home/fred ~/Floron
}

floron_unmount() {
  fusermount -u ~/Floron
  rmdir ~/Floron
}

neuron_mount() {
  mkdir -p ~/NeuronFTP
  ADDR="$(pw get Neuron:Common "%U:%P")@neuron://mnt"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/NeuronFTP
}

neuron_unmount() {
  fusermount -u ~/NeuronFTP
  rmdir ~/NeuronFTP
}

phone_mount_ssw() {
  mkdir -p ~/PhoneFTP
  ADDR="$(pw get Phone "%U:%P")@phonessw:9999"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/PhoneFTP
}

phone_mount() {
  mkdir -p ~/PhoneFTP
  ADDR="$(pw get Phone "%U:%P")@phone:9999"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/PhoneFTP
}

phone_unmount() {
  fusermount -u ~/PhoneFTP
  rmdir ~/PhoneFTP
}

alias vpn='/usr/bin/openpyn de -t 5 -f -m 40'
alias vpn_p2p='vpn --p2p'
alias vpn_kill='sudo /usr/bin/openpyn -x'

alias largest="find . -type f -printf '%s %p\n' | sort -nr | head -20"

DLSUB_CMD="subdl -i --output={m}.{L}.{S}"
DLSUB_CMD="$DLSUB_CMD $(pw get OpenSubtitles "--username %U --password %P")"
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
