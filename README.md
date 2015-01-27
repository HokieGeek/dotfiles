dotfiles
========
My personal linux config files

![](https://dl.dropboxusercontent.com/u/6524/ss.png)

## Installation
``` sh
git clone --recursive https://github.com/HokieGeek/dotfiles
cd dotfiles
./setup.sh
```

## Features

### vim
The following plugins are being used: https://github.com/HokieGeek/MahVimPlugins/blob/master/README.md

#### List of commands and mappings defined in the vimrc
##### Commands
Scratch

##### Mappings
| Mapping | What it does |
| ------- | ------------ |
| gh | :Scratch |
| gsh | :Split |
| gsv | :Vsplit |
| gw | ctrl-w |
| ga | :ArgsToggle |
| gc | ctrl-] |
| go | :TListToggle |
| \\\\ | :Grep |
| g\ | :Grep -b |
| g/ | :Grep -a |
| &lt;F9&gt; | Save session |
| &lt;leader&gt;&lt;F9&gt; | Save all window sessions |
| &lt;F10&gt; | Delete session |
| &lt;leader&gt;&lt;F10&gt; | Load session |
| &lt;F12&gt; | Cycle colorschemes |
| &lt;leader&gt;&lt;F12&gt; | Set specific colorscheme (herald) |
| &lt;count&gt;]b | Next buffer |
| &lt;count&gt;[b | Previous buffer |
| ]B | Last buffer |
| [B | First buffer |
| &lt;count&gt;]a | Next argument |
| &lt;count&gt;[a | Previous argument |
| ]A | Last argument |
| [A | First argument |
| &lt;count&gt;]q | Next quickfix item |
| &lt;count&gt;[q | Previous quickfix item |
| ]Q | Last quickfix item |
| [Q | First quickfix item |
| &lt;count&gt;]l | Next location item |
| &lt;count&gt;[l | Previous location item |
| ]L | Last location item |
| [L | First location item |
| con | Toggle relative number (if supported, else toggles number) |
| coc | Toggles cursor line |
| coC | Toggles cursor column (if supported) |
| cow | Toggles word wrap |
| cos | Toggles spelling |
| col | Toggles displaying of special characters (tabs) |
| cox | Toggles syntax highlighting |
| cot | Toggles status bar |
| cob | Toggles background (light/dark) |
| coh | Toggles search highlighting |
| cop | Toggles paste mode |
| coF | Toggles folding |
| cof | Enables syntax folding |
| coff | Enables manual folding |
| cofff | Enables marker folding |
| jk | &lt;esc&gt; |
| kj | &lt;esc&gt; |
| ZZ | :wqa |
| ZQ | :qa! |
| &lt;space&gt; | : |
| Y | Copy whole line |
| n | (nzz) Centers screen on search hit |
| jj | ctrl-n |
| kk | ctrl-p |

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

