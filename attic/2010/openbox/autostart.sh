# xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 -f -F -I.1 -O.1 &
feh --bg-scale /personal/themes/wallpaper1.jpg &
tint2 &
xscreensaver -no-splash &
thunar --daemon &
xpad &
nm-applet &
trayfreq &

# Run XDG autostart things.  By default don't run anything desktop-specific
# See xdg-autostart --help more info
DESKTOP_ENV=""
if which /usr/lib/openbox/xdg-autostart >/dev/null 2>&1; then
  /usr/lib/openbox/xdg-autostart $DESKTOP_ENV
fi

