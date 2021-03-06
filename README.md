dotfiles
========
My personal linux config files

![](https://dl.dropboxusercontent.com/u/6524/ss.png)

## Installation
``` sh
git clone --recursive https://github.com/HokieGeek/dotfiles
dotfiles/setup.sh
```

## Philosophy
I prefer to KISS as much as possible. All I need is a terminal and a browser and I've got all I need for 99% of what I do. I prefer smaller laptops so I try to take advantage of as much real estate as I can.

I keep the look and feel minimal so that it's less distracting so, to that end, the "color scheme" is dark with some gray shadings and a single color used by tmux and the conky bar up top. The script `bin/schemecolor` will sed the indicated color into my tmux.conf and xmonad.hs and will reload both so that I can change the color on-the-fly.

## Features

### chrome
Running a dark theme and using the cVim plugin to use vim bindings. This makes me very happy.

### vim
The following plugins are being used:

#### Mine
| Repo | Description |
| ---- | ----------- |
| [vit](https://gitlab.com/HokieGeek/vit) | |
| [¿que?](https://gitlab.com/HokieGeek/que) | |
| [comentarista](https://gitlab.com/HokieGeek/comentarista) | |
| [shai-hulud](https://gitlab.com/HokieGeek/shai-hulud) | |
| [argh](https://gitlab.com/HokieGeek/argh) | |
| [sessioner](https://gitlab.com/HokieGeek/sessioner) | |
| [splitter](https://gitlab.com/HokieGeek/splitter) | |
| [uvix](https://gitlab.com/HokieGeek/uvix) | |
| [unembed](https://gitlab.com/HokieGeek/unembed) | |
| [vim-ng](https://gitlab.com/HokieGeek/vim-ng) | |

#### Third-party
| Workflow-intrusive | Non-intrusive |
| ------------------ | ------------- |
| [kien/ctrlp.vim](https://github.com/kien/ctrlp.vim) | [matchit](http://www.vim.org/scripts/script.php?script_id=39 "vim.org #39") |
| [mbbill/undotree](https://github.com/mbbill/undotree.git) | [tpope/vim-pathogen](https://github.com/tpope/vim-pathogen) |
| [tommcdo/vim-exchange](https://github.com/tommcdo/vim-exchange) | [tpope/vim-repeat](https://github.com/tpope/vim-repeat) |
| [tpope/vim-surround](https://github.com/tpope/vim-surround) | [tpope/vim-endwise](https://github.com/tpope/vim-endwise.git) |
| [michaeljsmith/vim-indent-object](https://github.com/michaeljsmith/vim-indent-object) | [tpope/vim-markdown](https://github.com/tpope/vim-markdown) |
| [welle/targets](https://github.com/wellle/targets.vim.git) | [elzr/vim-json](https://github.com/elzr/vim-json.git) |
| [junegunn/vim-easy-align](https://github.com/junegunn/vim-easy-align) | [fatih/vim-go](https://github.com/fatih/vim-go.git) |
| [SirVer/ultisnips](https://github.com/SirVer/ultisnips.git) | [derekwyatt/vim-scala](https://github.com/derekwyatt/vim-scala.git) |
| [taglist](http://www.vim.org/scripts/script.php?script_id=273 "vim.org #273") | [leafgarland/typescript-vim](https://github.com/leafgarland/typescript-vim) |
| [ynkdir/vim-vimlparser](https://github.com/ynkdir/vim-vimlparser) | |
| [syngan/vim-vimlint](https://github.com/syngan/vim-vimlint) | |

#### List of new commands and mappings

##### Commands

| Command | Source plugin | What it does |
| ------- | ------------- | ------------ |
| Only | argh | |
| ArgsOnly | argh | |
| ArgsToggle | argh | |
| ArgsAll | argh | |
| Badd | argh | |
| Bd | argh | |
| Build | shai-hulud | |
| Split | splitter | |
| Vsplit | splitter | |
| Run | splitter | |
| RunIn | splitter | |
| Log | splitter | |
| Find | uvix | |
| Chmod | uvix | |
| Rm | uvix | |
| Tail | uvix | |
| Grep | uvix | |
| Git | vit | |
| Helptags | vim-pathogen | |
| SyntasticInfo | syntastic | |
| EasyAlign | vim-easy-align | |
| LiveEasyAlign | vim-easy-align | |

##### Mappings

| Mapping | Source plugin | What it does |
| ------- | ------------- | ------------ |
| g&lt;Tab&gt; | comentarista | |
| g&lt;shift&gt;&lt;Tab&gt; | comentarista | |
| cs | vim-surround | |
| ds | vim-surround | |
| yss | vim-surround | |
| cx | vim-exchange | |
| cxx | vim-exchange | |
| X | vim-exchange | |
| cxc | vim-exchange | |

## Text-objects

| Object | Source plugin | What it does |
| ------ | ------------- | ------------ |
| ai | vim-indent-object | |
| ii | vim-indent-object | |
| al | vim-indent-object | |


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

Also using `dmenu` for some application launchers as well as the selector which lets me change the color theme.

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
