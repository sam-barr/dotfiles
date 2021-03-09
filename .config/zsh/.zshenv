typeset -U PATH path
path=("$HOME/.cargo/bin" "$HOME/.local/bin" "$path[@]")
export PATH

export GDK_SCALE=2

export EDITOR=/usr/bin/nvim

# use vim to view man pages
export MANPAGER='nvim +Man!'
