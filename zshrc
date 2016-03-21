[[ $- != *i* ]] && return
[[ -z "$TMUX" && -f "/usr/bin/tmux" ]] && exec tmux

# Completion {{{
autoload -Uz compinit; compinit
setopt complete_in_word   # Allow tab completion in the middle of a word
setopt always_to_end      # When completing from the middle of a word, move the cursor to the end of the word
setopt auto_list          # Automatically list choices
setopt list_ambiguous     # Complete a word up to where it becomes ambiguous
setopt auto_remove_slash  # Remove an inserted slash when not needed
setopt complete_aliases # ??

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} 
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

[ -f $HOME/.shell-plugins/zsh-autosuggestions/autosuggestions.zsh ] && { 
    source $HOME/.shell-plugins/zsh-autosuggestions/autosuggestions.zsh
    zle -N autosuggest-start
}

[ -e /usr/share/doc/pkgfile/command-not-found.zsh ] && source /usr/share/doc/pkgfile/command-not-found.zsh

setopt correct # Spelling correction of commands
# }}}

# History {{{
HISTFILE=~/.histfile
HISTSIZE=1024
SAVEHIST=1024
setopt append_history        # Append to the history file
setopt inc_append_history    # Add to history file as commands are typed
setopt share_history         # All zsh sessions share the same history
setopt hist_ignore_all_dups  # Do not add dupes
setopt hist_ignore_space     # Ignore commands which start with a space
setopt hist_no_store         # Remove the history command from the command history
# }}}

# Vimify {{{
export EDITOR="nvim"
setopt vi

# Allows editing the command line in
bindkey -M vicmd v edit-command-line
autoload edit-command-line; zle -N edit-command-line
# }}}

# Term Colors {{{
# export TERM="xterm-256color"
# [ -n "$TMUX" ] && export TERM="screen-256color"

# case $TERM in
#     xterm*)
#         precmd () {print -Pn "\e]0;%~\a"}
#         ;;
# esac
# export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
# export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
# export LESS_TERMCAP_me=$'\E[0m'           # end mode
# export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
# export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
# export LESS_TERMCAP_ue=$'\E[0m'           # end underline
# export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
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

alias fuckit="autoload -U tetris && tetris" # heh

# Changing directories
setopt auto_cd           # Just use the name of a directory to CD to it
setopt auto_pushd        # Uses pushd instead of cd
setopt pushd_ignore_dups # Don't push multiple copies of the same directory to the stack
setopt pushd_to_home     # pushd without an argument goes to home
setopt pushd_silent      # Do not print directory stack after pushd or popd

# Expansions
setopt extendedglob      # Use advanced expansion (#,~,^)
setopt nomatch           # Print error when filename pattern doesn't match

setopt hist_subst_pattern # Allow pattern matching with :s
unsetopt beep

# Job control
# unsetopt notify           # Wait until a prompt needs to be printing before notifying that bg job has stopped
unsetopt bg_nice          # Run bg'ed processes at regular priority

autoload zmv # Fancy mv command
# }}}

# Source the things {{{
. $HOME/.aliases
. $HOME/.shell-plugins/prompt.zsh
[ -f $HOME/.zshrc_work ] && . $HOME/.zshrc_work
# }}}

# vim: set ft=sh foldmethod=marker
