typeset -U PATH path
path=("$HOME/.cargo/bin" "$HOME/.local/bin" "$HOME/.gem/ruby/2.7.0/bin" "$path[@]")
export PATH

export EDITOR=/usr/bin/nvim

# use vim to view man pages
export MANPAGER='nvim +Man!'
