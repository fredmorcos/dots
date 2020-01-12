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
alias g='grep'
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

alias n-sysctl='ssh root@neuron systemctl'
alias n-reboot='n-sysctl reboot'
alias n-poweroff='n-sysctl poweroff'
alias n-up='ssh root@neuron pacman -Syu'
alias kodi-restart='n-sysctl restart kodi'
alias kodi-stop='n-sysctl stop kodi'

wmount() {
  mkdir -p ~/Windows
  sudo mount /dev/nvme0n1p6 ~/Windows
}

wunmount() {
  sudo umount ~/Windows
  rmdir ~/Windows
}

dmount() {
  sudo cryptsetup open /dev/disk/by-label/EncryptedData data
  mkdir -p ~/Data
  if [ "$1" == "check" ]; then
    sudo btrfs check -p /dev/mapper/data
  fi
  sudo mount -o defaults,rw,relatime,compress=zstd /dev/mapper/data ~/Data
}

dunmount() {
  sudo umount ~/Data
  rmdir ~/Data
  sudo cryptsetup close data
  sudo hdparm -y /dev/disk/by-label/EncryptedData
}

fmount() {
  mkdir -p ~/Floron
  sshfs fred@floron:/home/fred ~/Floron
}

funmount() {
  fusermount -u ~/Floron
  rmdir ~/Floron
}

nmount() {
  mkdir -p ~/Neuron
  ADDR="$(pw get Neuron:Common "%U:%P")@neuron://mnt"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/Neuron
}

nunmount() {
  fusermount -u ~/Neuron
  rmdir ~/Neuron
}

pmountssw() {
  mkdir -p ~/PhoneFTP
  ADDR="$(pw get Phone "%U:%P")@phonessw:9999"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/PhoneFTP
}

pmount() {
  mkdir -p ~/PhoneFTP
  ADDR="$(pw get Phone "%U:%P")@phone:9999"
  curlftpfs -o connect_timeout=10 "$ADDR" ~/PhoneFTP
}

punmount() {
  fusermount -u ~/PhoneFTP
  rmdir ~/PhoneFTP
}

alias vpn='/usr/bin/openpyn de -t 5 -f -m 40'
alias vpn-p2p='vpn --p2p'
alias vpn-kill='sudo /usr/bin/openpyn -x'

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

alias gitstatus='gitall status -s'
alias gitpp='gitall pull -q && gitall push -q'
