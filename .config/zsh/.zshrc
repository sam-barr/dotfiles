# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep notify
unsetopt extendedglob nomatch
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '$ZDOTDIR/.zshrc'

autoload -Uz compinit
compinit -d $ZDOTDIR/.zcompdump
# End of lines added by compinstall

unalias run-help
autoload -Uz run-help

#variable definitions
TIMEFMT=$'\nreal\t%*E\nuser\t%*U\nsys\t%*S\nmem\t%Mkb'
PROMPT='%B%F{green}%m%f:%F{blue}%~%f%b%(!.#.$) '
eval $(dircolors -b)
#end variable definitions

#completion stuff
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
#end completion stuff

#aliases from bash
alias ls='ls --color=always'
alias ll='ls -Alh'

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

alias config="/usr/bin/git --git-dir=/home/sam-barr/.config/dotfiles/ --work-tree=/home/sam-barr"

alias rootshell="sudo -Es"
#end aliases

source $ZDOTDIR/zsh-vim-mode/zsh-vim-mode.plugin.zsh

neofetch
