/usr/local/bin/VBoxClient --clipboard
/usr/local/bin/VBoxClient --display
/usr/local/bin/VBoxClient --seamless

for i in /usr/local/lib/X11/fonts/*; do
  xset fp+ $i
done

xset fp rehash

exec /usr/local/bin/i3
