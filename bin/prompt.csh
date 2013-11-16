#!/bin/csh
alias precmd 'set v=$?; if ($v <= 0) set js=":)"; '\
             'if ($v == 42) set js="%{\033[1;32m%};)%{\033[0m%}"; '\
             'if ($v > 0 && $v != 42) set js="%{\033[1;31m%}:( $v%{\033[0m%}"; '\
             'set gb=""; set numMods=0; git status >& /dev/null; set v=$?; '\
             'if ($v <= 0) set gb=" %{\033[35m%}["`git branch | grep "^*" | sed "s/^\*\s*//"`"%{\033[0m%}"; '\
             'if ($v <= 0) set numMods=`git status -s -uno | egrep -c "(A|M|D)"`; '\
             'if ($v <= 0 && $numMods > 0) set gb="${gb}%{\033[1m%}%{\033[33m%} ${numMods}%{\033[0m%}%{\033[35m%}"; '\
             'if ($v <= 0) set gb="${gb}%{\033[0m%}%{\033[35m%}]%{\033[0m%}"; '\
             'set pwdlen=`pwd | wc -c`; set pwdlim=55; '\
             'set prompt="${js} %B%n@%m:%b%/${gb}> "; '\
             'if ($pwdlen > $pwdlim) set prompt="${gb}%{\033[1m%}%b%/%{\033[0m%} \n${js} %n@%m> "'
set prompt=":) %B%n@%m:%b%/> "
