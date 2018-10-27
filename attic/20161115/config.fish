set -U LANG       en_US.utf8
set -U LC_COLLATE C
set -U EDITOR     emacs

function fish_prompt
  echo -n -s (set_color green) ">>> " (set_color normal) \
  "[" (jobs | wc -l) "][" $status "] " \
  (set_color green) (prompt_pwd) (set_color normal) " "

  switch $USER
  case root
    echo -n -s (set_color green) "# " (set_color normal)
  case '*'
    echo -n -s "\$ "
  end
end

function fish_right_prompt
  echo -n -s (set_color yellow) "[" \
  (date "+%H:%M | %Y-%m-%d") "]" (set_color normal)
end

alias em   "emacs"
alias grep "grep --color"
alias ls   "ls --color"
alias ll   "ls -lh --group-directories-first"
alias la   "ll -A"
alias mv   "mv -i"
alias cp   "cp -i"
alias up   "yaourt -Syu"

set kodi_servs "kodi udisks polkit upower avahi-dnsconfd avahi-daemon.service"

alias neuron_systemctl    "ssh neuron sudo systemctl"
alias neuron_reboot       "neuron_systemctl reboot"
alias neuron_poweroff     "neuron_systemctl poweroff"
alias neuron_kodi_restart "neuron_systemctl restart kodi"
alias neuron_kodi_stop    "neuron_systemctl stop $kodi_servs"
alias neuron_rsyncd_start "neuron_systemctl start rsyncd"
alias neuron_rsyncd_stop  "neuron_systemctl stop rsyncd"

function weather
  curl http://wttr.in/$argv
end

alias weatherlz "weather linz"

alias cpu_command   "sudo cpupower -c all frequency-set -g"
alias cpu_powersave "cpu_command powersave"
alias cpu_powerperf "cpu_command performance"
alias cpu_powerdyn  "cpu_command ondemand"
