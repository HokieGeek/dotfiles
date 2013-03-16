#!/bin/bash

prompt_pwdlim=40
prompt_lastReturnVal() {
    ret=$1
    val="\033[32;1m:)"
    [ $ret -gt 0 ] && val="\033[31;1m:("
    [ $ret -eq 42 ] && val="\033[34m;)"
    echo -e "${val}\033[0m"
}

prompt_repoInfo() {
    git status >/dev/null 2>&1
    ret=$?
    [ $ret -le 0 ] && {
        echo -en "\033[35m<"

        # Put in the branch
        branch=`git branch | grep "^*" | sed "s/^\*\s*//"`
        echo -n "${branch}"

        # Add the number of modified files
        numMods=`git status -s -uno | egrep -c "(A|M|D)"`
        [ $numMods -gt 0 ] && echo -en "\033[01;33m ${numMods}\033[0m"

        echo -e "\033[35m>\033[0m "
    }
}

prompt_jobs() {
    # Should be bold white on red: \e[01;37;40m]
    # :
    numJobs=`jobs | wc -l`
    [ $numJobs -gt 0 ] && echo "\033[01;37;40m[${numJobs}]\033[0m"
}

prompt_display() {
    pwdlen=$(pwd | wc -c)
    [ $pwdlen -gt $prompt_pwdlim ] \
        && echo "$pwdlen 2>" \
        || echo "$pwdlen 1> "

    # # && echo '`prompt_repoInfo`\[\e[37;1m\]\w\[\e[0m\]\n`prompt_lastReturnVal` \[\e[37m\]\u@\h\[\e[0m\] [\!]> ' \
    # # || echo '`prompt_lastReturnVal` \[\e[37m\]\u@\h:\[\e[1m\]\w\[\e[0m\] `prompt_repoInfo`[\!]> '
}
PS1=`prompt_display`

#PS1='`prompt_lastReturnVal` \[\e[37m\]\u@\h:\[\e[1m\]\w\[\e[0m\] `prompt_repoInfo`[\!]> '
#PROMPT_COMMAND='PS1="`prompt_lastReturnVal` \[\e[37m\]\u@\h:\[\e[1m\]$(if [[ `pwd | wc -c` > $prompt_pwdlim ]]; then echo "\\W"; else echo "\\w"; fi)\[\e[0m\] `prompt_repoInfo`[\!]> "'
PROMPT_COMMAND='val=$?;PS1="\[\e[1m\]$(if [[ `pwd | wc -c` > $prompt_pwdlim ]]; then echo "\\W"; else echo "\\w"; fi)\[\e[0m\] `prompt_repoInfo` \n`prompt_lastReturnVal $val` "'
# PROMPT_COMMAND='val=$?;PS1="$(printf "%*s\r%s" $(( COLUMNS-1 )) "$(prompt_jobs)" "\[\e[1m\]$(if [[ `pwd | wc -c` > $prompt_pwdlim ]]; then echo "\\W"; else echo "\\w"; fi)\[\e[0m\] `prompt_repoInfo` \n`prompt_lastReturnVal $val` ")"'
