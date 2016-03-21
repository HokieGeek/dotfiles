[[ $- != *i* ]] && return
[[ -z "$TMUX" && -f "/usr/bin/tmux" ]] && exec tmux

# Completion {{{
autoload -Uz compinit; compinit

zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle :compinstall filename '$HOME/.zshrc'
#allow tab completion in the middle of a word
setopt complete_in_word

[ -f $HOME/.shell-plugins/zsh-autosuggestions/autosuggestions.zsh ] && { 
    source $HOME/.shell-plugins/zsh-autosuggestions/autosuggestions.zsh
    zle -N autosuggest-start
}

[ -e /usr/share/doc/pkgfile/command-not-found.zsh ] && source /usr/share/doc/pkgfile/command-not-found.zsh

setopt correct # Spelling correction of commands
# }}}

# History {{{
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=1000
setopt append_history inc_append_history share_history hist_ignore_all_dups hist_ignore_space
# }}}

# Vimify {{{
export EDITOR="nvim"
bindkey -v
bindkey -M vicmd v edit-command-line

autoload edit-command-line; zle -N edit-command-line
# }}}

# Term Colors {{{
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
# }}}

# PATHing {{{
export GOPATH=${HOME}/src/go
export PATH=${PATH}:/sbin:/usr/sbin:/usr/local/sbin
export PATH=${PATH}:${GOPATH}/bin:~/.bin
# }}}

# Misc {{{
## Get notified when someone logs in
watch=all                       # watch all logins
logcheck=30                     # every 30 seconds
WATCHFMT="%n from %M has %a tty%l at %T %W"

autoload -U tetris && tetris

setopt autocd auto_pushd pushd_ignore_dups
setopt extendedglob nomatch
unsetopt beep notify

autoload zmv # Fancy mv command
# }}}

# Source the things {{{
. $HOME/.aliases
. $HOME/.shell-plugins/prompt.zsh
[ -f $HOME/.zshrc_work ] && . $HOME/.zshrc_work
# }}}

# vim: set ft=sh foldmethod=marker
