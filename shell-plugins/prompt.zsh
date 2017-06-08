prompt_cvsInfo() {
    info=" %{$fg[magenta]%}<CVS>"

    # Add a symbol if modified files
    status_out=`cvs -n -q update`
    numMods=`echo ${status_out} | egrep -c "(A|M|D)"`
    [ $numMods -gt 0 ] && info=${info}"%{$fg[red]%}±%{$reset_color%} "

    echo "${info}%{$reset_color%}"
}

prompt_mercurialInfo() {
    info=" %{$fg[cyan]%}"

    # Determine the branch
    branch=`hg branch 2>/dev/null`
    info="${info}${branch}"

    status_out=`hg status 2>/dev/null`

    # Add indicator if there are modified files
    numMods=`echo $status_out | egrep -c "(A|M|D)"`
    [ $numMods -gt 0 ] && info=${info}"%b%F{52}±%{$reset_color%} "

    echo "${info}%{$reset_color%}"
}

prompt_gitInfo() {
    info=" %{$fg[yellow]%}"

    # Determine the branch
    branch=`git branch 2>/dev/null | grep "^*" | sed "s/^\*\s*//"`
    info="${info}${branch}"

    [ "`git stash list | wc -l`" -gt 0 ] && info=${info}"%{$fg[blue]%}∾%{$reset_color%}"

    status_out=`git status --porcelain -sb -uno 2>/dev/null`

    # Add indicator if repository is behind remote
    isBehind=`echo $status_out | grep -c "##.*\[.*[,\s]*behind.*\]"`
    [ $isBehind -gt 0 ] && info=${info}"%{$fg_no_bold[yellow]%}◂%{$reset_color%}"
    # U+25C2 - ◂

    # Add indicator if there are unpushed changes
    isAhead=`echo $status_out | grep -c "##.*\[ahead.*\]"`
    [ $isAhead -gt 0 ] && info=${info}"%{$fg_no_bold[yellow]%}▸%{$reset_color%}"
    # U+25B8 - ▸

    # Add indicator if there are modified files
    numMods=`echo $status_out | egrep -c "(A|M|D)"`
    [ $numMods -gt 0 ] && info=${info}"%{$fg[red]%}±%{$reset_color%} "

    echo -n "${info}%{$reset_color%}"
}

prompt_repoInfo() {
    which git >/dev/null 2>&1 && git rev-parse --is-inside-work-tree >/dev/null 2>&1 && prompt_gitInfo
    which hg >/dev/null 2>&1 && hg status >/dev/null 2>&1 && prompt_mercurialInfo
    which cvs >/dev/null 2>&1 && cvs status >/dev/null 2>&1 && prompt_cvsInfo
}

prompt_jobs() {
    [ `jobs | wc -l` -gt 0 ] && echo "%{$fg[red]%}•%{$reset_color%}" || echo " "
}

prompt_pwd() {
    echo -n "%F{238}%~%{$reset_color%}"
}

prompt_host() {
    [ -n "$SSH_CLIENT" ] && echo -n "%{$fg[green]%}@%m%{$reset_color%} "
}

prompt_gotmail() {
    # U+2709
    echo -n "✉"
}

autoload -U promptinit && promptinit
autoload -U colors && colors
setopt prompt_subst

local last_ret="%(?, ,%{$fg[red]%}•%{$reset_color%})"

# U+256D - ╭ ; U+2570 - ╰  ; U+2574 - ╴
PROMPT='%F{234}╭%f${last_ret}$(prompt_host)$(prompt_pwd)$(prompt_repoInfo)
%F{234}╰╴%f$(prompt_jobs)'
# RPROMPT="$(prompt_gotmail)" 
