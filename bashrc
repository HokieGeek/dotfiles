### DEFAULT STUFF
# If not running interactively, don't do anything
[[ -z "$PS1" ]] && return
[[ $- != *i* ]] && return
[ -z "$SSH_CLIENT" -a -z "$TMUX" ] && exec tmux

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Add an "alert" alias for long running commands.  Use like so: sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

[ -f ~/.aliases ] && . ~/.aliases

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

### MY STUFF
set -o vi
ulimit -c unlimited

HISTFILESIZE=20000
HISTSIZE=10000
HISTCONTROL=ignoredups
HISTIGNORE="ls"

shopt -s checkwinsize
shopt -s histappend
# Combine multiline commands into one in history
shopt -s cmdhist

export PATH=${PATH}:${HOME}/.bin
export EDITOR="nvim"

. $HOME/.bin/prompt.sh
