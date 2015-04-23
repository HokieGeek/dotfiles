#!/bin/bash

prompt_gitInfo() {
    # echo -en ' \[\e[35m\]'
    echo -n ' \[\e[01;36m\]'

    # Put in the branch
    branch=`git branch | grep "^*" | sed "s/^\*\s*//"`
    echo -n "${branch}"

    status_out=`git --porcelain status -sb -uno 2>/dev/null`

    # Add a symbol if local needs to be updated with remote
    isBehind=`echo ${status_out} | grep -c "##.*\[behind.*\]"`
    [ $isBehind -gt 0 ] && echo -n '\[\e[01;35m\]◂\[\e[0m'


    # Add a symbol if changes need to be pushed
    isAhead=`echo ${status_out} | grep -c "##.*\[ahead.*\]"`
    [ $isAhead -gt 0 ] && echo -n '\[\e[01;35m\]▸\[\e[0m'

    # Add a symbol if modified files
    numMods=`echo ${status_out} | egrep -c "(A|M|D)"`
    [ $numMods -gt 0 ] && echo -n '\[\e[01;31m\]±\[\e[0m\] '
}

prompt_repoInfo() {
    `git status >/dev/null 2>&1` && prompt_gitInfo
}

prompt_lastReturnVal() {
    [ $1 -gt 0 ] && echo -n '\[\e[31m\]☹\[\e[0m\] '
}

prompt_jobs() {
    [ `jobs | wc -l` -gt 0 ] && echo -n '\[\e[01;31m\]¡\[\e[0m\] '
}

prompt_userHost() {
    [ -n "$SSH_CLIENT" ] && echo -n '\[\e[35m\]@\h\[\e[0m\] '
}

prompt_pwd() {
    # echo -n '\[\e[33;1m\]\w\[\e[0m\]'

    echo -n '\[\e[33;1m\]'
    top=$(dirname "$(dirname $HOME)")
    trueHome=`readlink $top``echo $HOME | sed "s;$top;;"`
    [ $(echo `pwd` | grep -c $trueHome) -gt 0 ] && echo -n $(echo `pwd` | sed "s;$trueHome;~;") || echo -n '\w'
    echo -n '\[\e[0m\]'
}

prompt_display() {
    echo 'val=$?;PS1="\[\e[37m\]╭\[\e[0m\] $(prompt_lastReturnVal $val)$(prompt_userHost)$(prompt_pwd)$(prompt_repoInfo)\n\[\e[37m\]╰─\[\e[0m\]$(prompt_jobs) "'
}
PROMPT_COMMAND=$(prompt_display)
# PROMPT_COMMAND='val=$?;PS1="$(prompt_jobs)$(prompt_userHost)$(prompt_pwd)$(prompt_repoInfo)\n$(prompt_lastReturnVal $val) "'
