# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep notify
unsetopt extendedglob nomatch
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/sam-barr/.zshrc'

autoload -Uz compinit
compinit -d $ZDOTDIR/.zcompdump
# End of lines added by compinstall

PROMPT='%B%F{green}%m%f:%F{blue}%~%f%b%(!.#.$) '

#aliases from bash
alias ls='ls --color=auto'
alias ll='ls -Al'

alias vim=nvim
alias svim="sudo -E nvim"
alias vsplit="vim -O"
alias :wq="echo You\'re not in vim dumbass"
alias :q="echo You\'re not in vim dumbass"
alias :w="echo You\'re not in vim dumbass"

alias present="zathura --mode=presentation --page=0"
alias clear='clear && neofetch'
alias internet="ping 8.8.8.8"

alias cinstall="cargo install --path . --root ~/.local --force --locked"

alias config="/usr/bin/git --git-dir=/home/sam-barr/dotfiles/ --work-tree=/home/sam-barr"
#end aliases

neofetch
