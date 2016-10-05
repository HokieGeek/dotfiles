[[ $- != *i* ]] && return
[[ -z "$TMUX" && -f "/usr/bin/tmux" ]] && exec tmux

# Completion {{{
autoload -Uz compinit && compinit # Turn on tab completion
# autoload -Uz promptinit && promptinit # Prompt themes
setopt complete_in_word   # Allow tab completion in the middle of a word
setopt always_to_end      # When completing from the middle of a word, move the cursor to the end of the word
setopt auto_list          # Automatically list choices
setopt list_ambiguous     # Complete a word up to where it becomes ambiguous
setopt auto_remove_slash  # Remove an inserted slash when not needed
setopt complete_aliases   # ??

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' menu select=2
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} 
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:functions' ignored-patterns '_*' # Don't complete on commands I don't have

# Disable completion on CVS files
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# Prettier kill auto completion
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

zstyle ':completion:*:cd:*' ignore-parents parent pwd

[ -f $HOME/.shell-plugins/zsh-autosuggestions/autosuggestions.zsh ] && { 
    source $HOME/.shell-plugins/zsh/zsh-autosuggestions/autosuggestions.zsh
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
setopt hist_ignore_all_dups  # Do not add dupes
setopt hist_ignore_space     # Ignore commands which start with a space
setopt hist_no_store         # Remove the history command from the command history
# }}}

# Vimify {{{
export EDITOR="vim"
setopt vi

bindkey -M vicmd v edit-command-line
bindkey -M vicmd '?' history-incremental-search-backward
zle -N edit-command-line
autoload -Uz edit-command-line

# allow ctrl-p, ctrl-n for navigate history (standard behaviour)
bindkey '^P' up-history
bindkey '^N' down-history

# allow ctrl-h, ctrl-w, ctrl-? for char and word deletion (standard behaviour)
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
# }}}

# Term Colors {{{
# Colorized man pages
export LESS_TERMCAP_mb=$(printf "\e[1;31m")     # being blinking
export LESS_TERMCAP_md=$(printf "\e[1;34m")     # begin bold
export LESS_TERMCAP_me=$(printf "\e[0m")        # end mode
export LESS_TERMCAP_se=$(printf "\e[0m")        # end standout-mode
export LESS_TERMCAP_so=$(printf "\e[1;44;33m")  # begin standout-mode - info box
export LESS_TERMCAP_ue=$(printf "\e[0m")        # end underline
export LESS_TERMCAP_us=$(printf "\e[1;32m")     # begin underline
# }}}

# PATHing {{{
export GOPATH=${HOME}/go
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

## Fixes backspace "issue"
function zle-line-init () { echoti smkx }
function zle-line-finish () { echoti rmkx }
zle -N zle-line-init
zle -N zle-line-finish
# }}}

# Source the things {{{
. $HOME/.aliases
. $HOME/.shell-plugins/prompt.zsh
[ -f $HOME/.zshrc_alt ] && . $HOME/.zshrc_alt
# }}}

# vim: set ft=sh foldmethod=marker:
