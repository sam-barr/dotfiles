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
alias ls='exa --color=always --group-directories-first'
alias ll='exa --color=always --group-directories-first -al'

alias vim=nvim
alias vsplit="vim -O"

alias present="zathura --mode=presentation --page=0 --config-dir=/"
alias internet="ping -c5 8.8.8.8"

alias cinstall="cargo install --path . --force --locked"

alias config="/usr/bin/git --git-dir=/home/sam-barr/.config/dotfiles/ --work-tree=/home/sam-barr"
alias bash="HISTFILE=/dev/null bash" # don't create a history file when using bash
alias mv="mv -i"
alias cp="cp -i"
alias ?='echo $?'

alias disasm='objdump -drRwC -Mintel --no-show-raw-insn'

alias fix-pacman='rm /var/lib/pacman/db.lck'

#end aliases

export KEYTIMEOUT=5
source $ZDOTDIR/zsh-vim-mode/zsh-vim-mode.plugin.zsh
