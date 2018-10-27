# Put system-wide fish configuration entries here
# or in .fish files in conf.d/
# Files in conf.d can be overridden by the user
# by files with the same name in $XDG_CONFIG_HOME/fish/conf.d

# This file is run by all fish instances.
# To include configuration only for login shells, use
# if status --is-login
#    ...
# end
# To include configuration only for interactive shells, use
# if status --is-interactive
#   ...
# end

function fish_prompt
  set -l username (whoami)

  set -l prompt_sign '$'
  set -l prompt_sign_color (set_color normal)

  if string match -a "root" $username
    set prompt_sign '#'
    set prompt_sign_color (set_color brred)
  end

  printf '%s%s%s[%s][%s] %s %s%s%s ' \
    (set_color brgreen) '>' (set_color normal) \
    (jobs -p | wc -l) \
    $status \
    (prompt_pwd) \
    $prompt_sign_color $prompt_sign (set_color normal)
end

set LANG           en_US.UTF-8
set LC_COLLATE     C
set EDITOR         emacs
set HISTCONTROL    ignoredups
set TERM           xterm-256color
set fish_term24bit 1

# shopt -s autocd cdspell checkwinsize cmdhist dirspell histappend

alias grep  "grep --color"
alias ls    "ls --color"
alias ll    "ls -lh --group-directories-first"
alias la    "ll -A"
alias mv    "mv -i"
alias cp    "cp -i"
alias up    "yaourt -Syu"
alias upall "up --devel --noconfirm"
alias dired "emacs -nw ."
alias em    "emacs"
alias e     "emacs"

set -l kodi_services "kodi udisks polkit upower avahi-dnsconfd avahi-daemon.service"

alias neuron-systemctl    "ssh neuron sudo systemctl"
alias neuron-reboot       "neuron-systemctl reboot"
alias neuron-poweroff     "neuron-systemctl poweroff"
alias neuron-kodi-restart "neuron-systemctl restart kodi"
alias neuron-kodi-stop    "neuron-systemctl stop $kodi_services"

alias dlsub "subdl -i --output=\"%m.%L.%S\""
alias yt480 "youtube-dl -f '[height<=480]'"
alias yt720 "youtube-dl -f '[height<=720]'"

function weather
  curl "http://wttr.in/$argv"
end

alias weatherlz "weather linz"

alias cpupow-cmd  "sudo cpupower -c all frequency-set -g"
alias cpupow-save "cpupow-cmd powersave"
alias cpupow-perf "cpupow-cmd performance"
alias cpupow-dyn  "cpupow-cmd ondemand"

alias disk-poweroff "udisksctl power-off --block-device $argv[1]"

alias x11record "cnee --record --mouse -o \"$argv[1]\" -sk q -t 2"
alias x11replay "cnee --replay -f \"$argv[1]\" -sk q"

# alias gitsub "git submodule foreach --recursive git"

function gitall
  for i in *
    if test -e "$i/.git"; or test -d "$i/.git"
      cd "$i"
      echo ">>> git $argv: $i"
      git $argv
      cd ..
    end
  end
end

function find-archives
  find . \
    -iname "*.rar*"  -or \
    -iname "*.tar"   -or \
    -iname "*.tar.*" -or \
    -iname "*.zip*"  -or \
    -iname "*.7z*"   -or \
    -iname "*.tpz"
end

function find-lossless-music
  find . \
    -iname "*.wav*"  -or \
    -iname "*.flac*" -or \
    -iname "*.m4a*"  -or \
    -iname "*.m4v*"  -or \
    -iname "*.wma*"
end
