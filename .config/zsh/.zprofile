if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
else
    echo Systemctl: $(systemctl is-active graphical.target), expected: active
    echo Display: \"$DISPLAY\", expected: \"\"
    echo \$XDG_VTNR: $XDG_VTNR, expected: 1
    echo conditions for startx not met
fi
