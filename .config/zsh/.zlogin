if [[ $XDG_VTNR == "1" ]]; then
    echo -n "Update pacman packages? [yN] "
    read REPLY
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo ""
        sudo pacman -Syu
        update-aur
    fi
    echo -n "startx? [yN] " 
    read REPLY
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo ""
        startx
    fi
fi
