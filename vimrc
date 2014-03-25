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
    let g:goldenview__enable_at_startup = 0
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

""" Highlights {{{
function! SetMyHighlights()
    " echomsg "SetMyHighlights()"
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
endfunction
" }}}

""" Sessions {{{
""" Some logic that allows me to create a session with the current layout
if exists("b:session_file")
    unlet! b:session_file
endif
" TODO: env var / function that retrieves the .vim dir?
function! InitiateSession()
    let l:sessions_dir = $HOME."/.vim/sessions"
    if isdirectory(l:sessions_dir) == 1
        call system("mkdir ".l:sessions_dir)
    endif
    let b:session_file = l:sessions_dir."/".expand("%:t").".session"
    unlet! l:sessions_dir
endfunction
autocmd BufWinEnter * call InitiateSession()
function! SaveSession()
    if exists("b:session_file")
        execute "mksession! ".b:session_file
        if filereadable(b:session_file)
            redraw
            echo "Saved session"
        else
            echohl WarningMsg
            echomsg "Error saving session"
            echohl None
        endif
    endif
endfunction
function! LoadSession()
    if exists("b:session_file")
        if filereadable(b:session_file)
            execute "source ".b:session_file
            syntax on
            redraw
            echo "Loaded saved session"
        endif
    endif
endfunction
function! DeleteSession()
    if exists("b:session_file")
        if filereadable(b:session_file)
            call delete(b:session_file)
            echo "Deleted session"
        else
            echo "No session found"
        endif
    endif
endfunction
autocmd VimEnter * call LoadSession()
" }}}

""" Views {{{
if ! &diff
    func! MakeViewOnLeave()
        " TODO: split into two au's but need to ensure order
        if exists("g:loaded_output")
            call LoadedContentClear()
        endif
        if expand("%") != ""
            mkview!
        endif
    endfun
    autocmd BufWinLeave * call MakeViewOnLeave()
    autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
endif
" }}}

""" Functions {{{
"" Split the screen {{{
function! TmuxSplitHere(vertical, size)
    let l:cmd = "tmux split-window"
    " if tmux -V > 1.4
        let l:cmd .= " -c ".expand("%:p:h")
    " endif
    if (a:vertical == 1)
        let l:cmd .= " -h"
    endif
    if (a:size > 0)
        let l:cmd .= ((a:vertical == 1) ? " -p " : " -l ").a:size
    endif
    call system(l:cmd)
endfunction

function! ScreenSplitHere(vertical, size)
    let l:screen_cmd = "screen -dr ".expand("$STY")." -X"

    let l:cmd = l:screen_cmd." split ".((a:vertical == 1) ? "-v" : "")
    let l:cmd .= " && ".l:screen_cmd." focus"
    if (a:size > 0)
        let l:cmd .= " && ".l:screen_cmd." resize ".a:size
    endif
    let l:cmd .= " && ".l:screen_cmd." chdir ".expand("%:p:h")
    let l:cmd .= " && ".l:screen_cmd." screen"
    call system(l:cmd)
endfunction

function! SplitHere(vertical, size)
    if exists("$TMUX")
        call TmuxSplitHere(a:vertical, a:size)
    elseif exists("$TERM") && expand("$TERM") == "screen"
        call ScreenSplitHere(a:vertical, a:size)
    else
        echomsg "Did not find neither a tmux nor a screen session"
    endif
endfunction
" find files and populate the quickfix list (http://vim.wikia.com/wiki/VimTip799)
" fun! FindFiles(filename)
  " let error_file = tempname()
  " silent exe '!find . -name "'.a:filename.'" | xargs file | sed "s/:/:1:/" > '.error_file
  " set errorformat=%f:%l:%m
  " exe "cfile ". error_file
  " copen
  " call delete(error_file)
" endfun
" command! -nargs=1 FindFile call FindFiles(<q-args>)
" }}}

function! CycleColorScheme()
    if exists("g:my_schemes") == 0
        let g:my_schemes = split(glob(expand("$HOME/.vim/colors")."/*"), '\n')
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
endfunction
" }}}

""" Commands {{{
" Will allow me to sudo a file that is open without write permissions
cnoremap w!! %!sudo tee > /dev/null %
" }}}

""" Abbreviations {{{
" This is ridiculously useful
iabbrev datet- <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>
iabbrev date- <c-r>=strftime("%d/%m/%Y")<cr>
iabbrev time- <c-r>=strftime("%H:%M:%S")<cr>
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
" else
" TODO: just set grepprg to internal?
endif
" }}}

""" Keyboard mappings {{{
nnoremap <silent> <leader><leader> :nohlsearch<cr>
nnoremap Y y$

"" Session saving (et.al.)
nnoremap <silent> <F9> :call SaveSession()<cr>
nnoremap <silent> <leader><F9> :windo call SaveSession()<cr>
nnoremap <silent> <F10> :call DeleteSession()<cr>
nnoremap <silent> <leader><F10> :call LoadSession()<cr>
nnoremap <silent> <F12> :call CycleColorScheme()<cr>

"" Searching
" Current file
nnoremap <silent> g// :<c-u>noautocmd vimgrep // % <bar> cwindow<left><left><left><left><left><left><left><left><left><left><left><left><left>
nnoremap <silent> g/. :<c-u>noautocmd vimgrep /\<<c-r><c-w>\>/ % <bar> cwindow<cr>
" All open buffers
nnoremap <silent> g/\ :cexpr [] <bar> bufdo vimgrepadd //g % <bar> cwindow<left><left><left><left><left><left><left><left><left>
nnoremap <silent> g/, :cexpr [] <bar> bufdo vimgrepadd /<c-r><c-w>/g % <bar> cwindow<cr>
" All files in current directory and down
if exists("g:use_external_grep")
    nnoremap <silent> g\\ :<c-u>noautocmd grep  <bar> cinwdow<left><left><left><left><left>
    nnoremap <silent> g\. :<c-u>noautocmd grep <c-r><c-w> <bar> cinwdow<cr>
else
    nnoremap <silent> g\\ :<c-u>noautocmd vimgrep // ** <bar> cinwdow<left><left><left><left><left><left><left><left><left>
    nnoremap <silent> g\. :<c-u>noautocmd vimgrep /<c-r><c-w>/ ** <bar> cinwdow<cr>
endif

" A scratch space. Kinda useless, I think
nnoremap <silent> gh :botright new<bar>set buftype=nofile bufhidden=wipe nobuflisted noswapfile modifiable<bar>res 10<cr>
"" Split the term
nnoremap <silent> gsh :<c-u>call SplitHere(0, v:count)<cr>
nnoremap <silent> gsv :<c-u>call SplitHere(1, v:count)<cr>

" Ctrl+W is a horrible window control whatsit
nnoremap <silent> gw <c-w>

"" How are these not tied to a mapping already?
" This version of the buffer navigation keywords might be a bit more useful than the last
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

"" Configuration
nnoremap <silent> con :setlocal number! relativenumber!<cr>
nnoremap <silent> coc :setlocal cursorline!<cr>
nnoremap <silent> cow :setlocal wrap!<cr>
nnoremap <silent> cos :setlocal spell!<cr>
nnoremap <silent> col :setlocal list!<cr>
nnoremap <silent> cox :if exists("syntax_on")<bar>syntax off<bar>else<bar>syntax enable<bar>endif<cr>
nnoremap <silent> cot :if &laststatus == 2<bar>setlocal laststatus=1<bar>else<bar>setlocal laststatus=2<bar>endif<cr>
nnoremap <silent> cop :setlocal paste!<cr>
nnoremap <silent> cob :if &background == "dark"<bar>setlocal background=light<bar>else<bar>setlocal background=dark<bar>endif<cr>
nnoremap <silent> coh :setlocal hlsearch!<cr>
if exists("&colorcolumn")
    nnoremap <silent> coq :if &colorcolumn > 0<bar>setlocal colorcolumn=0<bar>else<bar>setlocal colorcolumn=81<bar>endif<cr>
endif

"" Plugins
if g:have_plugins
    nnoremap <silent> <leader>f :CtrlP<cr>
    nnoremap <silent> <leader>b :CtrlPBuffer<cr>
    nnoremap <silent> <leader>r :RainbowParenthesesToggle<cr>
    nnoremap <silent> <leader>t :TlistToggle<cr>
    map <leader>] <Plug>(expand_region_expand)
    map <leader>[ <Plug>(expand_region_shrink)
endif

"" Some (probably questionable) overrides/shortcuts
" FIXME: this doesn't work in paste mode
inoremap jk <esc>
inoremap kj <esc>

"" Completion
" word
inoremap jj <c-n>
inoremap JJ <c-p>
" line
inoremap kk <c-x><c-l>
" omni
inoremap KK <c-x><c-o>
" filename
" inoremap ?? <c-x><c-f>
" dictionary
" inoremap ?? <c-x><c-k>


nnoremap ZZ :wqa<cr>
nnoremap ZQ :qa!<cr>
nnoremap <space> :
vnoremap <space> :

"" I feel like being a pain in the ass
noremap <up> :echoerr "Use k instead! :-p"<cr>
noremap <down> :echoerr "Use j instead! :-p"<cr>
noremap <left> :echoerr "Use h instead! :-p"<cr>
noremap <right> :echoerr "Use l instead! :-p"<cr>
" }}}

""" Completion {{{
if filereadable(expand("$BUILD_CTAG_FILE"))
    autocmd FileType c,c++,cpp,h,h++,hpp,java execute "set tags=./tags,".expand("$BUILD_CTAG_FILE")."'"

    " Map some keys to access these
    nnoremap <silent> <leader>c :tab split<cr>:execute("tag ".expand("<cword>"))<cr>
else
    nnoremap <silent> <leader>c :echoerr "No tags file loaded"<cr>
endif
" }}}

""" Misc {{{
augroup MiscOptions
    autocmd!

    autocmd VimEnter,ColorScheme * call SetMyHighlights()

    " Turns on spell check when typing out long git commit messages
    autocmd BufNewFile,BufRead *.md set filetype=markdown
    autocmd BufNewFile,BufRead *.confluence set filetype=confluencewiki spell foldmethod=manual
    autocmd BufNewFile,BufRead *.conkyrc set filetype=conkyrc
    autocmd FileType gitcommit setlocal spell
    autocmd FileType markdown setlocal spell

    "" Stop asking about simultaneous edits.
    "" Copied from Damian Conway's lecture "More Instantly Better Vim" at OSCON 2013
    autocmd SwapExists * let v:swapchoice = 'o'
    autocmd SwapExists * echohl WarningMsg
    autocmd SwapExists * echomsg 'Duplicate edit session (readonly)'
    autocmd SwapExists * echohl None

    " Disable syntax highlight for files larger than 50 MB
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Turn off diffing
    autocmd VimLeave * windo diffoff

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END
" }}}

" vim: set foldmarker={{{,}}} foldmethod=marker formatoptions-=tc:
