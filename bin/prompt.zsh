#!/bin/zsh

prompt_gitInfo() {
    info=" %{$fg_bold[yellow]%}"

    # Put in the branch
    branch=`git branch | grep "^*" | sed "s/^\*\s*//"`
    info="${info}${branch}"

    status_out=`git status -sb -uno`

    # Add indicator if repository is behind remote
    isBehind=`echo $status_out | grep "##" | grep -c 'behind'`
    [ $isBehind -gt 0 ] && info=${info}"%{$fg_no_bold[yellow]%}◂%{$reset_color%}"
    # U+25C2 - ◂

    # Add indicator if there are unpushed changes
    isAhead=`echo $status_out | grep "##" | grep -c 'ahead'`
    [ $isAhead -gt 0 ] && info=${info}"%{$fg_no_bold[yellow]%}▸%{$reset_color%}"
    # U+25B8 - ▸

    # Add indicator if there are modified files
    numMods=`echo $status_out | egrep -c "(A|M|D)"`
    [ $numMods -gt 0 ] && info=${info}"%{$fg_bold[red]%}±%{$reset_color%} "

    echo "${info}%{reset_color%}"
}

prompt_repoInfo() {
    `git status >/dev/null 2>&1` && prompt_gitInfo
}

prompt_jobs() {
    [ `jobs | wc -l` -gt 0 ] && echo "%{$fg_bold[red]%}¡%{$reset_color%}"
}

prompt_pwd() {
    echo -n "%{$fg[white]%}%~%{$reset_color%}"
    # echo -n "%{$fg_bold[yellow]%}%~%{$reset_color%}"
}

prompt_host() {
    [ -n "$SSH_CLIENT" ] && echo -n "%{$fg[magenta]%}@%m%{$reset_color%} "
}

prompt_battery() {
    # Display alert on right prompt
    # ☠
    :
}

autoload -U promptinit && promptinit
autoload -U colors && colors
setopt prompt_subst

# local lastRet="%(?,,%{$fg_bold[red]%}☹%{$reset_color%})"
# local lastRet="%(?,,%{$bg[red]%}☹ %{$reset_color%})" # U+2639 - ☹
local lastRet="%(?,,%{$fg[red]%}∙%{$reset_color%})" # U+2639 - ☹

# U+256D - ╭ ; U+2570 - ╰ ; U+2500 - ─
PROMPT='%{$fg[black]%}╭%{$reset_color%}${lastRet} $(prompt_host)$(prompt_pwd)$(prompt_repoInfo)
%{$fg[black]%}╰─%{$reset_color%}$(prompt_jobs) '
RPROMPT='$(prompt_battery)'
