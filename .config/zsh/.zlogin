if [[ "$XDG_VTNR" == "1" ]]; then
    echo -n "Update pacman packages? [yN] "
    read REPLY
    if [[ "$REPLY" =~ ^[Yy]$ ]]; then
        while ! ping -c 1 -W 1 8.8.8.8 &> /dev/null; do
            echo "Waiting for internet..."
            sleep 1
        done
        echo ""
        sudo pacman -Syu
        update-aur

        echo -n "Recompile git packages? [yN] "
        read REPLY
        if [[ "$REPLY" =~ ^[Yy]$ ]]; then
            echo ""
            sudo recompile-aur-git
        fi
    fi

    if [[ -L /dev/disk/by-label/sam-barr-hdd ]]; then
        echo -n "Do you want to mount your hard drive? [yN] "
        read REPLY
        if [[ "$REPLY" =~ ^[Yy]$ ]]; then
            echo "Mounting..."
            mount -L sam-barr-hdd
        fi
    fi

    echo -n "startx? [yN] " 
    read REPLY
    if [[ "$REPLY" =~ ^[Yy]$ ]]; then
        echo ""
        startx
    fi
fi
