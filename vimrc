set nocompatible " Not compatible with plain vi

""" Plugins {{{
filetype off

let g:have_plugins = 0
if has("vim_starting")
    if isdirectory(expand("$HOME/.vim/bundle/vim-pathogen"))
        set runtimepath+=$HOME/.vim/bundle/vim-pathogen
        let g:have_plugins = 1
    elseif isdirectory(expand("$HOME/vimfiles/bundle/vim-pathogen"))
        set runtimepath+=$HOME/vimfiles/bundle/vim-pathogen
        let g:have_plugins = 1
    endif
endif

if g:have_plugins
    execute pathogen#infect()
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
set wildignore=*.d,*.o,*.obj,*.bak,*.exe,*.swp,*~ " These file types are ignored when doing auto completions
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
set dictionary=/usr/share/dict/words
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
if exists("&relativenumber")
    set number relativenumber " I always turn these on
endif
if has("win32unix") " Cygwin can't handle it
    set laststatus=0
else
    set laststatus=2
endif
if $USER != "root"
    set modelines=1
endif
if exists('$TMUX')
    set clipboard=
else
    set clipboard=unnamed " Sync with OS clipboard
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
" find files and populate the quickfix list (http://vim.wikia.com/wiki/VimTip799)
" function! FindFiles(filename)
  " let error_file = tempname()
  " silent exe '!find . -name "'.a:filename.'" | xargs file | sed "s/:/:1:/" > '.error_file
  " set errorformat=%f:%l:%m
  " exe "cfile ". error_file
  " cwindow
" endfunction
" command! -nargs=1 Find call FindFiles(<q-args>)
" }}}

""" Commands {{{
" Will allow me to sudo a file that is open without write permissions
cnoremap w!! %!sudo tee > /dev/null %
" }}}

""" Abbreviations {{{
" This is ridiculously useful, particularly when taking notes
iabbrev datet- <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>
iabbrev date- <c-r>=strftime("%Y-%m-%d")<cr>
iabbrev time- <c-r>=strftime("%H:%M:%S")<cr>
iabbrev afp]] [AFP]<cr>
" }}}

""" Searching configuration {{{
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
    if g:have_plugins
        let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
        let g:ctrlp_use_caching = 0
    endif
    let g:use_external_grep = 1
elseif executable('ack')
    set grepprg=ack\ --nogroup\ ---nocolor
    let g:use_external_grep = 1
elseif executable('grep')
    set grepprg=grep\ -rnIH
endif
" }}}

""" Keyboard mappings {{{
"" Session saving (et.al.) " {{{
nnoremap <silent> <F9> :call sessioner#save()<cr>
nnoremap <silent> <leader><F9> :windo call sessioner#save()<cr>
nnoremap <silent> <F10> :call sessioner#delete()<cr>
nnoremap <silent> <leader><F10> :call sessioner#load()<cr>

nnoremap <silent> <F12> :NextColorScheme<cr>
nnoremap <silent> <leader><F12> :colorscheme herald<cr>
" }}}

"" Searching " {{{
" Current file
nnoremap <silent> \\ :<c-u>vimgrep // % <bar> cwindow<left><left><left><left><left><left><left><left><left><left><left><left><left>
nnoremap <silent> \. :<c-u>vimgrep /\<<c-r><c-w>\>/ % <bar> cwindow<cr>
" All open buffers
nnoremap <silent> g\\ :cexpr [] <bar> bufdo vimgrepadd //g % <bar> cwindow<left><left><left><left><left><left><left><left><left><left><left><left><left><left>
nnoremap <silent> g\. :cexpr [] <bar> bufdo vimgrepadd /<c-r><c-w>/g % <bar> cwindow<cr>
" All files in current directory and down
if exists("g:use_external_grep")
    nnoremap <silent> \/ :<c-u>silent grep  <bar> cwindow<left><left><left><left><left><left><left><left><left><left>
    nnoremap <silent> \, :<c-u>silent grep <c-r><c-w> <bar> cwindow<cr>
else
    nnoremap <silent> \/ :<c-u>noautocmd vimgrep // ** <bar> cwindow<left><left><left><left><left><left><left><left><left>
    nnoremap <silent> \, :<c-u>noautocmd vimgrep /<c-r><c-w>/ ** <bar> cwindow<cr>
endif
" }}}

"" Some user stuff " {{{
" A scratch space. Kinda useless, I think
nnoremap <silent> gh :botright new<bar>set buftype=nofile bufhidden=wipe nobuflisted noswapfile modifiable<bar>res 10<cr>
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
    nnoremap <silent> gb :CtrlPBuffer<cr>
    nnoremap <silent> go :TlistToggle<cr>
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
nnoremap <silent> ]t :<c-u>execute(v:count.'tnext')<cr>
nnoremap <silent> [t :<c-u>execute(v:count.'tprevious')<cr>
nnoremap <silent> ]T :tlast<cr>
nnoremap <silent> [T :tfirst<cr>
" }}}

"" Configuration " {{{
nnoremap <silent> con :setlocal number!<bar>if exists("&relativenumber")<bar>setlocal relativenumber!<bar>endif<cr>
if exists("&relativenumber")
    nnoremap <silent> coN :setlocal relativenumber!<cr>
endif
nnoremap <silent> coc :setlocal cursorline!<cr>
if exists("&colorcolumn")
    nnoremap <silent> coq :if &colorcolumn > 0<bar>setlocal colorcolumn=0<bar>else<bar>setlocal colorcolumn=81<bar>endif<cr>
endif
nnoremap <silent> cow :setlocal wrap!<cr>
nnoremap <silent> cos :setlocal spell!<cr>
nnoremap <silent> col :setlocal list!<cr>
nnoremap <silent> cox :if exists("syntax_on")<bar>syntax off<bar>else<bar>syntax enable<bar>endif<cr>
nnoremap <silent> cot :if &laststatus == 2<bar>setlocal laststatus=1<bar>else<bar>setlocal laststatus=2<bar>endif<cr>
nnoremap <silent> cob :if &background == "dark"<bar>setlocal background=light<bar>else<bar>setlocal background=dark<bar>endif<cr>
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
inoremap TT <c-x><c-]>
" }}}

"" I feel like being a pain in the ass " {{{
noremap <up> :echoerr "Use k instead! :-p"<cr>
noremap <down> :echoerr "Use j instead! :-p"<cr>
noremap <left> :echoerr "Use h instead! :-p"<cr>
noremap <right> :echoerr "Use l instead! :-p"<cr>
" }}}
" }}}

""" Misc {{{
augroup MiscOptions
    autocmd!

    " Filetype recognition
    autocmd BufNewFile,BufRead *.md set filetype=markdown
    autocmd BufNewFile,BufRead *.confluence set filetype=confluencewiki spell foldmethod=manual
    autocmd BufNewFile,BufRead *.conkyrc set filetype=conkyrc
    autocmd FileType markdown setlocal spell
    autocmd FileType gitcommit setlocal spell

    "" Stop asking about simultaneous edits.
    "" Copied from Damian Conway's lecture "More Instantly Better Vim" at OSCON 2013
    autocmd SwapExists * let v:swapchoice = 'o'
    autocmd SwapExists * echohl WarningMsg
    autocmd SwapExists * echomsg 'Duplicate edit session (readonly)'
    autocmd SwapExists * echohl None

    " Disable syntax highlight for files larger than 50 MB
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Turn off diffing when exiting
    autocmd VimLeave * windo diffoff

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    "" Views {{{
    if ! &diff
        autocmd BufWinLeave * if expand("%") != "" | mkview! | endif
        autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
    endif
" }}}

augroup END
" }}}

" vim: set foldmarker={{{,}}} foldmethod=marker formatoptions-=tc:
