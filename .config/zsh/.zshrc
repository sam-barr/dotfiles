HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep notify
unsetopt extendedglob nomatch

zstyle :compinstall filename '$ZDOTDIR/.zshrc'

autoload -Uz compinit
compinit -d $ZDOTDIR/.zcompdump

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

#aliases 
alias grep="echo Use rg; :" # I want to force myself to use rg
alias ls='ls --color=always'
alias ll='ls -Alh'

alias vim=nvim
alias vsplit="vim -O"
alias :wq="echo You're not in vim dumbass"
alias :q="echo You're not in vim dumbass"
alias :w="echo You're not in vim dumbass"

alias present="zathura --mode=presentation --page=0 --config-dir=~"
alias clear='clear && paleofetch'
alias internet="ping 8.8.8.8"

alias cinstall="cargo install --path . --root ~/.local --force --locked"

alias config="/usr/bin/git --git-dir=/home/sam-barr/.config/dotfiles/ --work-tree=/home/sam-barr"
alias bash="HISTFILE=/dev/null bash" # don't create a history file when using bash
alias mv="mv -i"
alias cp="cp -i"
#end aliases

source $ZDOTDIR/zsh-vim-mode/zsh-vim-mode.plugin.zsh

# fix systemctl completion?
_systemctl_unit_state() {
  typeset -gA _sys_unit_state
  _sys_unit_state=( $(__systemctl list-unit-files "$PREFIX*" | awk '{print $1, $2}') ) }

paleofetch
