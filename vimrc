set nocompatible " Not compatible with plain vi

if isdirectory(expand("$HOME/vimfiles"))
    let g:dot_vim_dir = expand("$HOME/vimfiles")
elseif isdirectory(expand("$HOME/.vim.afp"))
    let g:dot_vim_dir = expand("$HOME/.vim.afp")
else
    let g:dot_vim_dir = expand("$HOME/.vim")
endif

""" Plugins {{{
filetype off

let g:have_plugins = 0
if has("vim_starting") && isdirectory(expand(g:dot_vim_dir."/bundle/vim-pathogen"))
    execute "set runtimepath+=".g:dot_vim_dir."/bundle/vim-pathogen"
    let g:have_plugins = 1
endif

if g:have_plugins
    if has("win32unix") || has("win32") || has("win64") " Cygwin and windows can't handle it
        let g:que__vcs_section_enabled = 0
    endif
    execute pathogen#infect()
    let g:syntastic_javascript_checkers = ['jslint']
endif
" }}}

""" Options {{{
filetype plugin indent on

set autoindent " Indents when you insert
set tabstop=4 " Tab = 4 spaces. Because I'm not a savage
set softtabstop=4
set shiftwidth=4
set expandtab
set shiftround " Always indent to nearest tabstop
set smarttab " Use shiftwidth at left margin, instead of tabstops
set backspace=2 " Make backspace actually work, because why not?
set title
set noerrorbells
set visualbell
set ruler
set showcmd " Shows the command being typed
set noshowmode " Don't show -- INSERT --
set complete-=i " Don't search includes because they are slow
set wildmenu " Tab completion in command-line mode (:)
set wildignore=*.d,*.o,*.obj,*.bak,*.exe,*.swp,*~,tags " These file types are ignored when doing auto completions
set wildmode=list:longest,full
set viminfo=h,%,'50,"100,<10000,s1000,/1000,:1000 " Remembers stuff. RTFM
set history=1000
set undolevels=5000
if exists("&undofile")
    set undofile " Create an undo file so you can undo even after closing a file
endif
set foldenable " Close folds on open
set foldnestmax=5 " I think 5 nests is enough, thank you
set foldopen=insert,jump,mark,percent,quickfix,search,tag,undo " What movements open folds
set showmatch " Briefly jumps to matching bracket
set incsearch " Searches as you type
set ignorecase " I use \c enough times that this is the obvious choice
set smartcase " If search pattern uses upper case chars, make search case sensitive
set wrapscan " Searches wrap around the end of the file
set nowrap " I prefer this. I can always turn wrap on when its necessary with the mapping below
set linebreak " Wrap lines at convenient points
set list " Displays unprintable characters (whitespace, essentially)
" Define what symbols to use with unprintable characters
execute "set listchars=tab:>>,trail:\uB7,extends:#,nbsp:_"
set splitright " When doing a vertical split, it puts it to the right of the current window
set splitbelow " When doing a horizontal split, it puts it below the current window
set diffopt+=iwhite " Vimdiff will ignore whitespace diffs
set ttyfast " Smoother redrawing
set lazyredraw " Don't redraw during macros
set noscrollbind " Don't scroll windows synchronized
if filereadable("/usr/share/dict/words")
    set dictionary=/usr/share/dict/words
endif
set spellsuggest=best,15
set spelllang=en_us
set formatoptions+=n " Recognize numbered lists
set backup " Create a backup file
set backupdir=~/.tmp,~/tmp,/var/tmp,/tmp " Why not on .?
set writebackup " Make a backup before overwriting a file
set hlsearch
set hidden " You can change buffers without saving
set timeoutlen=400 " Let's see if this works for me
set ttimeoutlen=100 " Escape and others a bit faster
set noequalalways " Does not resize windows during a split or window close
set virtualedit=block " 'Square up' visual selections
set updatecount=20 " Save buffer every 20 characters
set scrolloff=2 " Scroll file when cursor is 2 lines from top or bottom
set sidescrolloff=4 " Scroll file horizontally when the cursor is 4 columns from left or right
set sidescroll=1 " Trying this out...
set textwidth=0 " Don't want automatic text width formatting
set modelines=1
set display=lastline
set number " I always turn these on
if exists("&relativenumber")
    set relativenumber
endif
execute "set laststatus=".(has("win32unix") ? 0 : 2)
execute "set clipboard=".(exists('$TMUX') ? "" : "unnamed")
" set shell=bash
" set shellpipe=2>&1\|tee
if has("win32") || has("win64") || has("win32unix")
    set shellslash
endif

" Always want it
set t_Co=256
set background=dark
if has("gui_running")
    try
        colorscheme badwolf
    catch /E185:/
        colorscheme murphy
    endtry
    " Never show the toolbar, menubar, right, left and bottom scrollbars
    set guioptions-=T guioptions-=m guioptions-=r guioptions-=l guioptions-=b
    set guioptions+=c " Use console dialogs instead of popup dialogs
else
    try
        colorscheme ir_black
    catch /E185:/
        colorscheme desert
    endtry
endif
syntax on
" }}}

""" Functions {{{
function! MyHighlights() " {{{
    " highlight CursorLine ctermbg=yellow ctermfg=black cterm=none
    highlight SpecialKey ctermbg=black ctermfg=lightgrey cterm=none

    " I like being able to spot my comments quickly
    highlight AFP ctermbg=darkblue ctermfg=red cterm=bold
    try
        call matchadd("AFP", "AFP")
        call matchadd("AFP", "afp")
    catch /E117:/
        match AFP /\cAFP/
    endtry

    " Make the completion menu actually visible
    highlight Pmenu ctermbg=white ctermfg=black
    highlight PmenuSel ctermbg=blue ctermfg=white cterm=bold
    highlight PmenuSbar ctermbg=grey ctermfg=grey
    highlight PmenuThumb ctermbg=blue ctermfg=blue

    if exists("&colorcolumn")
        highlight ColorColumn guibg=#C0C0C0 ctermbg=234
    endif
endfunction " }}}
function! CycleColorScheme() " {{{
    if !exists("g:my_schemes")
        let g:my_schemes = split(glob(expand(g:dot_vim_dir."/colors")."/*"), '\n')
        let g:my_schemes = map(g:my_schemes, 'fnamemodify(v:val, ":t:r")')
    endif
    if exists("g:my_current_scheme") > 0
        let l:idx = index(g:my_schemes, g:my_current_scheme)
    elseif exists("g:colors_name") > 0
        let l:idx = index(g:my_schemes, g:colors_name)
    else
        let l:idx = -1
    endif
    let l:idx += 1
    if l:idx > len(g:my_schemes)-1
        let l:idx = 0
    endif
    syntax reset
    let g:my_current_scheme = g:my_schemes[l:idx]
    set background=dark
    execute "colorscheme ".g:my_current_scheme
    echomsg "Switched to colorscheme: ".g:my_current_scheme
endfunction " }}}
" }}}

""" Commands {{{
command! -bar Scratch :botright new<bar>set buftype=nofile bufhidden=wipe nobuflisted noswapfile modifiable<bar>res 10<cr>
" }}}

""" Abbreviations {{{
" This is ridiculously useful, particularly when taking notes
iabbrev datet- <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>
iabbrev date- <c-r>=strftime("%Y-%m-%d")<cr>
iabbrev time- <c-r>=strftime("%H:%M:%S")<cr>

iabbrev afp]] [AFP]
" }}}

""" Keyboard mappings {{{
"" Session saving (et.al.) " {{{
nnoremap <silent> <F9> :call sessioner#save()<cr>
nnoremap <silent> <leader><F9> :windo call sessioner#save()<cr>
nnoremap <silent> <F10> :call sessioner#delete()<cr>
nnoremap <silent> <leader><F10> :call sessioner#load()<cr>

nnoremap <silent> <F12> :call CycleColorScheme()<cr>
nnoremap <silent> <leader><F12> :colorscheme herald<cr>
" }}}

"" Some user stuff " {{{
" A scratch space. Kinda useless, I think
nnoremap <silent> gh :Scratch<cr>
" nnoremap <silent> =" :Scratch<bar>put<bar>0d_<cr>
" Split the term
nnoremap <silent> gsh :Split<cr>
nnoremap <silent> gsv :Vsplit<cr>

" Ctrl+W is a horrible window control whatsit
nnoremap <silent> gw <c-w>
nnoremap <silent> ga :ArgsToggle<cr>
nnoremap <silent> gc <c-]>

"" Plugins
if g:have_plugins
    nnoremap <silent> gp :CtrlP<cr>
    nnoremap <silent> go :TlistToggle<cr>

    " Search current file
    nnoremap <silent> \\ :Grep 
    " Search all open buffers
    nnoremap <silent> g\ :Grep -b 
    " Search all files in current directory and down
    nnoremap <silent> g/ :Grep -a 
endif
" }}}

"" How are these not tied to a mapping already? " {{{
nnoremap <silent> ]b :<c-u>execute(v:count ? 'b '.v:count : 'bnext')<cr>
nnoremap <silent> [b :<c-u>execute(v:count ? 'b '.v:count : 'bprevious')<cr>
nnoremap <silent> ]B :blast<cr>
nnoremap <silent> [B :bfirst<cr>
" Argument
nnoremap <silent> ]a :<c-u>execute(v:count.'next')<cr>
nnoremap <silent> [a :<c-u>execute(v:count.'previous')<cr>
nnoremap <silent> ]A :last<cr>
nnoremap <silent> [A :first<cr>
" Quickfix
nnoremap <silent> ]q :<c-u>execute(v:count.'cnext')<cr>
nnoremap <silent> [q :<c-u>execute(v:count.'cprevious')<cr>
nnoremap <silent> ]Q :clast<cr>
nnoremap <silent> [Q :cfirst<cr>
" Location
nnoremap <silent> ]l :<c-u>execute(v:count.'lnext')<cr>
nnoremap <silent> [l :<c-u>execute(v:count.'lprevious')<cr>
nnoremap <silent> ]L :llast<cr>
nnoremap <silent> [L :lfirst<cr>
" Tags
" nnoremap <silent> ]t :<c-u>execute(v:count.'tnext')<cr>
" nnoremap <silent> [t :<c-u>execute(v:count.'tprevious')<cr>
" nnoremap <silent> ]T :tlast<cr>
" nnoremap <silent> [T :tfirst<cr>
" }}}

"" Configuration " {{{
if exists("&relativenumber")
    nnoremap <silent> con :setlocal relativenumber!<cr>
    nnoremap <silent> coN :setlocal number!<bar>if exists("&relativenumber")<bar>setlocal relativenumber!<bar>endif<cr>
else
    nnoremap <silent> con :setlocal number!<cr>
endif
nnoremap <silent> coc :setlocal cursorline!<cr>
if exists("&colorcolumn")
    nnoremap <silent> coC :if &colorcolumn > 0<bar>setlocal colorcolumn=0<bar>else<bar>setlocal colorcolumn=81<bar>endif<cr>
endif
nnoremap <silent> cow :setlocal wrap!<cr>
nnoremap <silent> cos :setlocal spell!<cr>
nnoremap <silent> col :setlocal list!<cr>
nnoremap <silent> cox :if exists("syntax_on")<bar>syntax off<bar>else<bar>syntax enable<bar>endif<cr>
nnoremap <silent> cot :let &laststatus = (&laststatus == 2 ? 1 : 2)<cr>
nnoremap <silent> cob :let &background = (&background == "dark" ? "light" : "dark")<cr>
" nnoremap <silent> cob :let &background = (&background == "dark" ? "light" : "dark")<bar>execute "colorscheme ".g:my_current_scheme<cr>
nnoremap <silent> coh :nohlsearch<cr>
nnoremap <silent> coH :setlocal hlsearch!<cr>
nnoremap <silent> cop :setlocal paste!<cr>
" }}}

"" Some (probably questionable) overrides/shortcuts " {{{
inoremap jk <esc>
inoremap kj <esc>

nnoremap ZZ :wqa<cr>
nnoremap ZQ :qa!<cr>
nnoremap <space> :
vnoremap <space> :

nnoremap Y y$
nnoremap n nzz
" }}}

"" Completion " {{{
" word
inoremap jj <c-n>
inoremap kk <c-p>
" line
inoremap JJ <c-x><c-l>
" filename
inoremap FF <c-x><c-f>
" dictionary
inoremap DD <c-x><c-k>
" user
inoremap UU <c-x><c-u>
" omni
inoremap KK <c-x><c-o>
" tag
" inoremap TT <c-x><c-]>
" }}}

"" I feel like being a pain in the ass " {{{
noremap <left> :echoerr "Use h instead! :-p"<cr>
noremap <right> :echoerr "Use l instead! :-p"<cr>
noremap <up> :echoerr "Use k instead! :-p"<cr>
noremap <down> :echoerr "Use j instead! :-p"<cr>
" noremap <down> ggdG:x<cr>
" }}}
" }}}

""" Misc {{{
augroup MiscOptions
    autocmd!

    autocmd VimEnter,ColorScheme * call MyHighlights()

    " Filetype recognition
    autocmd BufNewFile,BufRead *.md set filetype=markdown
    autocmd BufNewFile,BufRead *.confluence set filetype=confluencewiki spell foldmethod=manual
    autocmd BufNewFile,BufRead *.conkyrc set filetype=conkyrc
    autocmd FileType markdown setlocal spell
    autocmd FileType gitcommit setlocal spell
    autocmd Filetype make setlocal noexpandtab nolist

    "" Stop asking about simultaneous edits.
    "" Copied from Damian Conway's lecture "More Instantly Better Vim" at OSCON 2013
    autocmd SwapExists * let v:swapchoice = 'o' |
                       \ echohl WarningMsg |
                       \ echomsg 'Duplicate edit session (readonly)' |
                       \ echohl None |

    " Disable syntax highlight for files larger than 50 MB (taken from vim tips site)
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    " Turn off diffing when exiting otherwise the view stuff below won't work well
    autocmd VimLeave * windo diffoff

    " if ! &diff
        " autocmd BufWinLeave * if expand("%") != "" | mkview! | endif
        " autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
    " endif

    " Stole this straight out of tpope/vim-eunuch (and modified it a bit)
    autocmd BufNewFile * let b:brand_new_file = 1
    autocmd BufWritePost,FileWritePost *
        \ if exists('b:brand_new_file') |
        \   if getline(1) =~ '^#!' && executable('chmod') == 1 |
        \       silent! execute '!chmod +x "<afile>"' |
        \       edit |
        \   endif |
        \   unlet! b:brand_new_file |
        \ endif

    " Disable cursorline when in insert mode cause I don't really need that
    autocmd InsertEnter * let b:last_cursorline=&cursorline | set nocursorline
    autocmd InsertLeave * execute "let &cursorline=".b:last_cursorline

    autocmd Filetype qf setlocal number | if exists("&relativenumber") | setlocal norelativenumber | endif
augroup END
" }}}

" vim: set foldmarker={{{,}}} foldmethod=marker formatoptions-=tc:
