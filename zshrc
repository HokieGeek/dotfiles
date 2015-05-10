[[ -z "$TMUX" ]] && exec tmux

zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle :compinstall filename '/home/andres/.zshrc'
#zstyle :compinstall filename '/home/'`whoami`'/.zshrc'

autoload -Uz compinit
compinit

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=1000
setopt hist_ignore_all_dups hist_ignore_space

setopt autocd extendedglob nomatch
unsetopt appendhistory beep notify
bindkey -v
bindkey -M vicmd v edit-command-line

export TERM="xterm-256color"
[ -n "$TMUX" ] && export TERM="screen-256color"

case $TERM in
    xterm*)
        precmd () {print -Pn "\e]0;%~\a"}
        ;;
esac
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

setopt correct # Spelling correction of commands
autoload zmv # Fancy mv command
autoload edit-command-line; zle -N edit-command-line

export PATH=${PATH}:~/.bin
export EDITOR="nvim"

[ -e /usr/share/doc/pkgfile/command-not-found.zsh ] && source /usr/share/doc/pkgfile/command-not-found.zsh

. ~/.aliases
. ~/.shell-plugins/prompt.zsh
