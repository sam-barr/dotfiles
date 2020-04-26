typeset -U PATH path
path=("$HOME/.local/bin" "$path[@]")
export PATH

export EDITOR=/usr/bin/nvim

# use vim to view man pages
export MANPAGER='nvim +Man!'
