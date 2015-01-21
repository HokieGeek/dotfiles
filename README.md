dotfiles
========
My personal linux config files

![](https://dl.dropboxusercontent.com/u/6524/ss.png)

## Installation
``` sh
git clone https://github.com/HokieGeek/dotfiles
cd dotfiles
git submodule update --init --recursive
./setup.sh
```

## Features

### vim
The following plugins are being used: https://github.com/HokieGeek/MahVimPlugins/blob/master/README.md

#### List of commands and mappings defined in the vimrc
##### Commands
Scratch

##### Mappings
gh > :Scratch
gsh > :Split
gsv > :Vsplit
gw > ctrl-w
ga > :ArgsToggle
gc > ctrl-]
go > :TListToggle
\\ > :Grep
g\ > :Grep -b
g/ > :Grep -a

### urxvt
No scrollbar and transparent

### tmux
Simple statusbar that just shows currently open terminals and an identifier for the current pane

### shell prompts
Both **bash** and **zsh** prompts display git and mercurial repo information

### xmonad
A fairly basic setup with custom key bindings and use of the GS menu

### conky
Using it to display statistic such as IP addresses, CPU use, battery life, etc.

