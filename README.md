dotfiles
========
My personal linux config files

![](https://dl.dropboxusercontent.com/u/6524/ss.png)

## Installation
``` sh
git clone --recursive https://github.com/HokieGeek/dotfiles
cd dotfiles/vim/bundle
git submodule update --init
cd ../..
./setup.sh
```

## Philosophy
I prefer to KISS as much as possible. All I need is a terminal and a browser and I've got all I need for 99% of what I do. I prefer smaller laptops so I try to take advantage of as much real estate as I can.

I keep the look and feel minimal so that it's less distracting so, to that end, the "color scheme" is dark with some gray shadings and a single color used by tmux and the conky bar up top. The script `bin/schemecolor` will sed the indicated color into my tmux.conf and xmonad.hs and will reload both so that I can change the color on-the-fly.

## Features

### chrome
Running a dark theme and using the cVim plugin to use vim bindings. This makes me very happy.

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
| go | :TListToggle |
| ge | EasyAlign interactive mode |
| gc | Toggles Undotree |
| (visual)&lt;Enter&gt; | EasyAlign interactive mode |
| \\\\ | :Grep |
| g\ | :Grep -b |
| g/ | :Grep -a |
| &lt;F9&gt; | Save session |
| &lt;leader&gt;&lt;F9&gt; | Save all window sessions |
| &lt;F10&gt; | Delete session |
| &lt;leader&gt;&lt;F10&gt; | Load session |
| &lt;F12&gt; | Cycle colorschemes |
| &lt;leader&gt;&lt;F12&gt; | Set favorite colorscheme (differerent if on GUI vs. terminal) |
| &lt;C-F12&gt; | Sets to failsafe colorscheme (different if on GUI vs. terminal) |
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
| coN | Toggle relative number (if supported, else toggles number) |
| con | Toggles number and relative number |
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

### st
Nothing to be done here for simple terminal, of course, other than setting the font to DejaVu Sans Mono

### tmux
Simple statusbar that just shows currently open terminals and an identifier for the current pane.

Wrote a vim plugin that integrates with tmux somewhat.

It is exec'ed by the shell upon login.

### zsh
Ported my bash prompt which displays information such as git and mercurial repository status and flags if the previous command returned a non-zero value and/or if a process is currently backgrounded.

### xmonad
My focus was to enable layouts that let me take advantage of my small laptop screen in a way that most benefits what I am currently working on. I couple that with a good number of custom keybindings so that I don't have to use the mouse to interact with the window manager. I also have been making pretty heavy use of `dmenu` to provide me some extra control.

Using `dzen2` to display the workspaces.

Also using `dzen2` for some application launchers as well as the selector which lets me change the color theme.

##### A few sample custom key bindings that I love
| Binding | What it does |
| ------- | ------------ |
| Super+n | Switches to the next empty workspace |
| Super+Alt+n | Opens a browser window in the next available empty workspace |
| Super+Shift+n | Shifts active window to the next empty workspace |
| Super+Tab | Switches to the previous workspace |
| Super+Ctrl+[WS#] | Swaps current workspace with indicated workspace |

### conky + dzen2
Displaying statistics such as time, IP addresses, CPU use, battery life, etc.
