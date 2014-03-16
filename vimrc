set nocompatible " Not compatible with plain vi

""" Plugins {{{
filetype off

if has("vim_starting")
    set runtimepath+=~/.vim/bundle/vim-pathogen
endif

execute pathogen#infect()
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
set undofile " Create an undo file so you can undo even after closing a file
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
set number relativenumber " I always turn these on
if has("win32unix") || has("win32") || has("win64")
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
if has("gui_running")
    colorscheme solarized
    colorscheme badwolf
    set guioptions-=T " Never show the toolbar
    set guioptions-=m " Never show the menubar
    set guioptions-=r " Never show the right scrollbar
    set guioptions-=l " Never show the left scrollbar
    set guioptions-=b " Never show the bottom scrollbar
else
    colorscheme desert
endif
syntax on
" }}}

""" Highlights {{{
highlight CursorLine ctermbg=yellow ctermfg=black cterm=none
highlight SpecialKey ctermbg=black ctermfg=lightgrey cterm=none

highlight AFP ctermbg=darkblue ctermfg=red cterm=bold
call matchadd("AFP", "AFP")
call matchadd("AFP", "afp")

" Make the completion menu actually visible
highlight Pmenu ctermbg=white ctermfg=black
highlight PmenuSel ctermbg=blue ctermfg=white cterm=bold
highlight PmenuSbar ctermbg=grey ctermfg=grey
highlight PmenuThumb ctermbg=blue ctermfg=blue

highlight ColorColumn ctermbg=240
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
" function! DiffOrig()
    " mkview! 9
    " topleft vnew
    " set buftype=nofile bufhidden=wipe nobuflisted
    " execute "silent read ".expand("#")
    " 0d_
    " wincmd l
    " silent windo diffthis
    " windo set nomodifiable
    " 0
    " set modifiable syntax=off
" endfunction

"" Split the screen {{{
function! TmuxSplitHere(vertical, size)
    let l:cmd = "tmux split-window -c ".expand("%:p:h")
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

" function! ExplorerLeftPane()
    " TODO
" endfunction

" }}}

""" Commands {{{
" Will allow me to sudo a file that is open without write permissions
cnoremap w!! %!sudo tee > /dev/null %
" }}}

""" Abbreviations {{{
" This is ridiculously useful
iabbrev date- <c-r>=strftime("%d/%m/%Y %H:%M:%S")<cr>
" }}}

""" Keyboard mappings {{{
nnoremap <leader>s :source $MYVIMRC<cr>
nnoremap <silent> <leader><leader> :nohlsearch<cr>
nnoremap Y y$

"" Session saving (et.al.)
nnoremap <silent> <F9> :call SaveSession()<cr>
nnoremap <silent> <leader><F9> :windo call SaveSession()<cr>
nnoremap <silent> <F10> :call DeleteSession()<cr>
nnoremap <silent> <leader><F10> :call LoadSession()<cr>
nnoremap <silent> <F12> :colorscheme solarized<bar>colorscheme badwolf<cr>

"" Searching
" Current file
nnoremap <silent> g// :<c-u>noautocmd vimgrep // % <bar> cw<left><left><left><left><left><left><left><left>
nnoremap <silent> g/. :<c-u>noautocmd vimgrep /\<<c-r><c-w>\>/ % <bar> cw<cr>
" All open buffers
nnoremap <silent> g\\ :cex [] <bar> bufdo vimgrepadd //g % <bar> cw<left><left><left><left><left><left><left><left><left>
nnoremap <silent> g\. :cex [] <bar> bufdo vimgrepadd /<c-r><c-w>/g % <bar> cw<cr>
" All files in current directory and down
nnoremap <silent> g/\ :<c-u>noautocmd vimgrep // ** <bar> cw<left><left><left><left><left><left><left><left><left>
nnoremap <silent> g/, :<c-u>noautocmd vimgrep /<c-r><c-w>/ ** <bar> cw<cr>

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
nnoremap <silent> ]a :next<cr>
nnoremap <silent> [a :previous<cr>
nnoremap <silent> ]A :last<cr>
nnoremap <silent> [A :first<cr>
" Quickfix
nnoremap <silent> ]q :cnext<cr>
nnoremap <silent> [q :cprevious<cr>
nnoremap <silent> ]Q :clast<cr>
nnoremap <silent> [Q :cfirst<cr>
" Location
nnoremap <silent> ]l :lnext<cr>
nnoremap <silent> [l :lprevious<cr>
nnoremap <silent> ]L :llast<cr>
nnoremap <silent> [L :lfirst<cr>

" TODO: nnoremap <silent> ge :ExSidebar<cr>

"" Configuration
nnoremap <silent> con :setlocal number! relativenumber!<cr>
nnoremap <silent> coc :setlocal cursorline!<cr>
nnoremap <silent> cow :setlocal wrap!<cr>
nnoremap <silent> cos :setlocal spell!<cr>
nnoremap <silent> col :setlocal list!<cr>
nnoremap <silent> cox :if exists("syntax_on")<bar>syntax off<bar>else<bar>syntax enable<bar>endif<cr>
nnoremap <silent> cot :if &laststatus == 2<bar>setlocal laststatus=1<bar>else<bar>setlocal laststatus=2<bar>endif<cr>
nnoremap <silent> cop :setlocal paste!<cr>
nnoremap <silent> coq :if &colorcolumn > 0<bar>setlocal colorcolumn=0<bar>else<bar>setlocal colorcolumn=81<bar>endif<cr>
nnoremap <silent> cob :if &background == "dark"<bar>setlocal background=light<bar>else<bar>setlocal background=dark<bar>endif<cr>

" nnoremap <silent> Uo :call DiffOrig()<cr>

"" Plugins
nnoremap <silent> pu :GundoToggle<cr>
nnoremap <silent> pf :CtrlP<cr>
nnoremap <silent> pb :CtrlPBuffer<cr>
nnoremap <silent> pr :RainbowParenthesesToggle<cr>

"" Some (probably questionable) overrides/shortcuts
" FIXME: this doesn't work in paste mode
inoremap jj <esc>
inoremap jk <esc>
inoremap kj <esc>

"" Completion
" word
inoremap fg <c-x><c-n>
inoremap fG <c-x><c-p>
" line
inoremap sd <c-x><c-l>
" omni
inoremap cv <c-x><c-o>
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

" augroup Notes
    " autocmd!

    " autocmd BufRead,BufNewFile *
        " \ if &filetype == "" |
        " \   nnoremap <silent> <F5> :set filetype=notes<cr> |
        " \ endif
" augroup END
" }}}

""" Completion {{{
" if filereadable("??/tags")
    " autocmd FileType c,c++,cpp,h,h++,hpp set tags=./tags
    " autocmd BufwinEnter *.* echo "Loaded tags file"

    " Map some keys to access these
    " nnoremap <silent> <C-\> :tab split<cr>:execute("tag ".expand("<cword>"))<cr>
" else
    " nnoremap <silent> <C-\> :echoerr "No tags file loaded"<cr>
" endif

"" Use <C-X><C-O> to access these
augroup omni_complete
    autocmd!
    autocmd FileType c,c++,cpp,h,h++,hpp set omnifunc=ccomplete#Complete
    autocmd FileType java set omnifunc=javacomplete#Complete
    autocmd FileType python set omnifunc=pythoncomplete#Complete
    autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType css set omnifunc=csscomplete#CompleteCSS
    autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
    autocmd FileType php set omnifunc=pythoncomplete#CompletePHP
    autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
augroup END
" }}}

""" Misc {{{
augroup MiscOptions
    autocmd!

    "" Stop asking about simultaneous edits. 
    "" Copied from Damian Conway's lecture "More Instantly Better Vim" at OSCON 2013
    " TODO: This isn't working (not setting as readonly. Should also be not modifiable, I think)
    " autocmd SwapExists * let v:swapchoice = 'o'
    " autocmd SwapExists * echoerr 'Duplicate edit session (readonly)'

    " Turns on spell check when typing out long git commit messages
    autocmd FileType gitcommit setlocal spell
    autocmd BufNewFile,BufRead *.md set filetype=markdown
    autocmd FileType markdown setlocal spell

    " Disable syntax highlight for files larger than 50 MB
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Turn off diffing
    autocmd VimLeave * windo diffoff

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    autocmd BufRead,BufNewFile *.confluence set filetype=confluencewiki spell foldmethod=manual
augroup END
" }}}

" vim: set foldmarker={{{,}}} foldmethod=marker number relativenumber formatoptions-=tc:
