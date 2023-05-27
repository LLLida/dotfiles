#!/usr/bin/env sh
#First we allow ourselves to run herbstclient as hc
hc() { "${herbstclient_command[@]:-herbstclient}" "$@" ;}
#Next I am saying that $monitor represents all my monitors
monitor=${1:-0}
# Terminate already running bar instances
killall -q  polybar
# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
# Launch bars
polybar -c ~/.config/herbstluftwm/polybar.ini &
