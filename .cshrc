#!/bin/csh
bindkey -v
limit coredumpsize unlimited

set history = 1000
set filec
set autolist
setenv VISUAL vim

setenv NUM_PROCESSORS   `grep -c "processor" /proc/cpuinfo`
setenv ANT_HOME         /opt/apache-ant-1.8.3
setenv PATH ${PATH}:${ANT_HOME}:${ANT_HOME}/bin:$HOME/.bin

#-------------------------------------------------------------------------------
source /Scripts/git-completion.tcsh

if ( -r $HOME/.alias ) then
    source $HOME/.alias
endif

#-------------------------------------------------------------------------------
if ( (! $?ENVONLY) && $?prompt ) then
    source $HOME/.bin/prompt.csh
endif

## Setup dev aliases
if ( $?BUILD ) unset BUILD
source $HOME/.bin/switch_build.csh --create-aliases /work
# source /Scripts/AIC/switch_build.csh --create-aliases /work
