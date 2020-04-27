#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# I'm going to keep these here instead of in .zprofile or something
# because these depend on the X server already being started
"$HOME"/.local/bin/startup

# If DP-2 is connected, assume I'm plugged into my monitor
if xrandr | rg -q "DP-2 connected"
then
    display=acer
else
    display=dell
fi

"$HOME"/.local/bin/changeDisplay "$display"
export CURRENT_DISPLAY="$display"
exec xmonad
