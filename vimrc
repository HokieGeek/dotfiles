set nocompatible " Not compatible with plain vi

""" Options {{{
filetype off

execute pathogen#infect()

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
let m = matchadd("AFP", "AFP")
let m = matchadd("AFP", "afp")

" Make the completion menu actually visible
highlight Pmenu ctermbg=white ctermfg=black
highlight PmenuSel ctermbg=blue ctermfg=white cterm=bold
highlight PmenuSbar ctermbg=grey ctermfg=grey
highlight PmenuThumb ctermbg=blue ctermfg=blue
" }}}

""" Sessions {{{
""" Some logic that allows me to create a session with the current layout
if exists("s:session_file")
    unlet! s:session_file
endif
autocmd BufWinEnter * let s:session_file = $HOME."/.vim/sessions/".expand("%:t").".session"
function! SaveSession()
    exe "mksession! ".s:session_file
    if filereadable(s:session_file)
        redraw
        echo "Saved session"
    else
        echoerr "Error saving session"
    endif
endfunction
function! LoadSession()
    if filereadable(s:session_file)
        exe "source ".s:session_file
        syntax on
        redraw
        echo "Loaded saved session"
    endif
endfunction
function! DeleteSession()
    if filereadable(s:session_file)
        call delete(s:session_file)
        echo "Deleted session"
    else
        echo "No session found"
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

function! TmuxSplitHere(vertical, size)
    if exists("$TMUX")
        let l:cmd = "tmux split-window -c ".expand("%:p:h")
        if (a:vertical == 1)
            let l:cmd = l:cmd." -h"
        endif
        if (a:size > 0)
            if (a:vertical == 1)
                let l:cmd = l:cmd." -p ".a:size
            else
                let l:cmd = l:cmd." -l ".a:size
            endif
        endif
        call system(l:cmd)
    else
        echomsg "Not in a tmux session"
    endif
endfunction

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
nnoremap <silent> g\p :CtrlPBuffer<cr>
" All files in current directory and down
nnoremap <silent> g/\ :<c-u>noautocmd vimgrep // ** <bar> cw<left><left><left><left><left><left><left><left><left>
nnoremap <silent> g/, :<c-u>noautocmd vimgrep /<c-r><c-w>/ ** <bar> cw<cr>
nnoremap <silent> g/p :CtrlP<cr>

" Ctrl+W is a horrible window control whatsit
nnoremap <silent> gw <c-w>
" This version of the buffer navigation keywords might be a bit more useful than the last
nnoremap <silent> gb :<c-u>execute(v:count ? 'b '.v:count : 'bnext')<cr>
nnoremap <silent> gB :<c-u>execute(v:count ? 'b '.v:count : 'bprevious')<cr>
" A scratch space. Kinda useless, I think
" nnoremap <silent> gs :botright new<bar>set buftype=nofile noswapfile modifiable<bar>res 10<cr>
nnoremap <silent> gsh :<c-u>call TmuxSplitHere(0, v:count)<cr>
nnoremap <silent> gsv :<c-u>call TmuxSplitHere(1, v:count)<cr>

" TODO: nnoremap <silent> ge :ExSidebar<cr>

"" Configuration
nnoremap <silent> con :setlocal number! relativenumber!<cr>
nnoremap <silent> coc :setlocal cursorline!<cr>
nnoremap <silent> cow :setlocal wrap!<cr>
nnoremap <silent> cos :setlocal spell!<cr>
nnoremap <silent> col :setlocal list!<cr>
nnoremap <silent> cox :if exists("syntax_on")<bar>syntax off<bar>else<bar>syntax enable<bar>endif<cr>
nnoremap <silent> cot :if &laststatus == 2<bar>set laststatus=1<bar>else<bar>set laststatus=2<bar>endif<cr>
nnoremap <silent> cop :setlocal paste!<cr>

" nnoremap <silent> Uo :call DiffOrig()<cr>


"" Plugins
nnoremap <silent> pu :GundoToggle<cr>

" Some (probably questionable) overrides/shortcuts
" TODO: this doesn't work in paste mode
inoremap jk <esc>
inoremap kj <esc>
inoremap fs <c-n>
inoremap sf <c-n>

inoremap sd <c-x><c-l>
inoremap cv <c-x><c-o>

nnoremap ZZ :wqa<cr>
nnoremap ZQ :qa!<cr>
nnoremap <space> :
vnoremap <space> :

"" I feel like being a pain in the ass
noremap <up> :echoerr "Use k instead! :-p"<cr>
noremap <down> :echoerr "Use j instead! :-p"<cr>
noremap <left> :echoerr "Use h instead! :-p"<cr>
noremap <right> :echoerr "Use l instead! :-p"<cr>

augroup Notes
    autocmd!

    autocmd BufRead,BufNewFile *
        \ if &filetype == "" |
        \   nnoremap <silent> <F5> :set filetype=notes<cr> |
        \ endif
augroup END
" }}}

""" Status line {{{
"" Highlights {{{
highlight SL_HL_Default ctermbg=233 ctermfg=249 cterm=none
highlight SL_HL_Mode ctermbg=55 ctermfg=7 cterm=bold
highlight SL_HL_PasteWarning ctermbg=140 ctermfg=232 cterm=bold

highlight SL_HL_FileNotModifiedNotReadOnly ctermbg=233 ctermfg=249 cterm=none
highlight SL_HL_FileNotModifiedReadOnly ctermbg=233 ctermfg=88 cterm=bold
highlight SL_HL_FileModifiedNotReadOnly ctermbg=22 ctermfg=7 cterm=none
highlight SL_HL_FileModifiedReadOnly ctermbg=22 ctermfg=196 cterm=bold

highlight SL_HL_FileNotModifiableNotReadOnly ctermbg=88 ctermfg=232 cterm=bold
highlight SL_HL_FileNotModifiableReadOnly ctermbg=88 ctermfg=9 cterm=bold

highlight SL_HL_FileTypeIsUnix ctermbg=233 ctermfg=239 cterm=none
highlight SL_HL_FileTypeNotUnix ctermbg=52 ctermfg=233 cterm=none

highlight SL_HL_CapsLockWarning ctermbg=118 ctermfg=232 cterm=bold

highlight SL_HL_FileInfo ctermbg=234 ctermfg=244 cterm=none
highlight SL_HL_FileInfoTotalLines ctermbg=234 ctermfg=239 cterm=none
" }}}
function! GetStatusLine()
    let l:statusline="%#SL_HL_mode#\ %{mode()}\ %#SL_HL_Default#"
    if &paste == 1
        " TODO: Paste ▶
        let l:statusline.="%#SL_HL_PasteWarning# PASTE %#SL_HL_Default#"
    endif
    " nnoremap <silent> cow :setlocal wrap!<cr>
    " nnoremap <silent> cos :setlocal spell!<cr>

    " File name, type and modified
    " TODO: if filename > 0
    let l:filename = expand("%:t")
    if len(l:filename) > 0
        let l:statusline.="\ "
        if &modifiable == 1
            if &modified == 1
                if &readonly == 0
                    let l:statusline.="%#SL_HL_FileModifiedNotReadOnly#"
                else
                    let l:statusline.="%#SL_HL_FileModifiedReadOnly#"
                endif
            else
                if &readonly == 0
                    let l:statusline.="%#SL_HL_FileNotModifiedNotReadOnly#"
                else
                    let l:statusline.="%#SL_HL_FileNotModifiedReadOnly#"
                endif
            endif
        else
            if &readonly == 0
                let l:statusline.="%#SL_HL_FileNotModifiableNotReadOnly#"
            else
                let l:statusline.="%#SL_HL_FileNotModifiableReadOnly#"
            endif
        endif
        let l:statusline.="\ ".l:filename."\ "
    endif

    if len(&filetype) > 0
        if &fileformat == 'unix'
            let l:statusline.="%#SL_HL_FileTypeIsUnix#"
        else
            let l:statusline.="%#SL_HL_FileTypeNotUnix#"
        endif
        let l:statusline.="\ ".&filetype."\ "
    endif

    " Display git info
    let l:statusline.=vit#StatusLine()

    " Right-justify the rest
    let l:statusline.="%#SL_HL_Default#"
    let l:statusline.="%="

    " Syntastic flag
    let l:statusline.="%#warningmsg#"
    let l:statusline.="%{SyntasticStatuslineFlag()}"
    let l:statusline.="%#SL_HL_Default#"

    " TODO: This gets expensive
    " let l:capsState = system("xset -q | grep \"Caps Lock\" | awk '{ print $2$3$4 }'")
    " if match(l:capsState, "on") > -1
        " let l:statusline.="%#SL_HL_CapsLockWarning# CAPS %#SL_HL_Default#"
    " endif

    let l:statusline.="%#SL_HL_FileInfo#\ %l%#SL_HL_FileInfoTotalLines#/%L%#SL_HL_FileInfo#"
    let l:statusline.=",%c\ %P"

    let l:statusline.="%*"

    return l:statusline
endfunction
set statusline=%!GetStatusLine()
set laststatus=2

" }}}

""" Commenting {{{
function! SLCOtoggle() "{{{
    normal ma
    if getline(".") =~ '^\s*'.g:slco " Remove the comment tag it contains
        silent exe ":.s;".escape(g:slco, "[]")." *;;"
        normal `a
        exe "silent normal ".eval(strlen(g:slco)+1)."h"
    else " Comment line
        exe "normal I".g:slco." "
        normal `a
        exe "normal ".eval(strlen(g:slco)+1)."l"
    endif

    if exists("g:slcoE")
        if getline(".") =~ g:slcoE.'\s*$'  " Remove the end comment tag it contains
            silent exe ":.s; *".escape(g:slcoE, "[]").";;"
            normal `a
            " normal 3h
            exe "silent normal ".eval(strlen(g:slcoE)+1)."h"
        else " Comment line (end)
            exe "normal ^A ".g:slcoE." "
            normal `a
        endif
    endif
    nohlsearch
endfunction "}}}
function! BLKCOtoggle(isVisual) "{{{
    "" Define a few variables
    let curl = line(".")
    let curc = col(".")
    let startpat = escape(g:blkcoS, "*[]\{}")
    let endpat = escape(g:blkcoE, "*[]\{}").'\s*$'

    " Search entire file 'upwards' for either a S or an E. if E or no S, then do comment
    if match(getline("."), '^\s*'.startpat) > -1
        let found = curl
    else
        if match(getline("."), endpat) > -1
            call cursor(curl-1, 0)
        endif
        let found = search(startpat."\\|".endpat, "bW")
    endif
    " echo "found #".found.": ".getline(found)

    if found > 0 && match(getline(found), endpat) == -1 " Delete existing comments
        if a:isVisual == 0 && searchpair(startpat, '', endpat, 'W') > 0
            normal dd
            call cursor(found, 0)
            normal dd
        endif
    else " Create new comment block
        if a:isVisual > 0
            call cursor(line("'<"), 0)
            let mark=">"
        else
            call cursor(curl, curc)
            let mark="a"
        endif

        let marked=line("'".mark)
        if marked > 0
            if marked > curl
                exe "normal O".g:blkcoS."'".mark."o".g:blkcoE.""
            else
                exe "normal o".g:blkcoE."'".mark."O".g:blkcoS.""
            endif
        else
            exe "normal o".g:blkcoE."kO".g:blkcoS.""
        endif
    endif
    call cursor(curl, curc)
endfunction "}}}

" if exists("g:slco") | unlet! slco | endif
if exists("g:slcoE") | unlet! slcoE | endif
" if exists("g:blkcoS") | unlet! blkcoS | endif
" if exists("g:blkcoE") | unlet! blkcoE | endif
augroup commenting
    autocmd!
    autocmd FileType vim,vimrc let slco="\"" " vim
    autocmd FileType sql,haskell let slco="--"    " SQL and Haskell
    autocmd FileType ahk let slco=";"        " AutoHotkey
    " Java/C/C++
    autocmd FileType java,c,c++,cpp,h,h++,hpp
        \ let slco="//" |
        \ let blkcoS="/*" |
        \ let blkcoE="*/"
    " Shell/Scripts
    autocmd FileType sh,ksh,csh,tcsh,zsh,bash,dash,pl,python,make,gdb
        \ let slco="#"
    " XML
    autocmd FileType xml,html
        \ let slco="<![CDATA[-----" |
        \ let slcoE="-----]]>" |
        \ let blkcoS="<![CDATA[-------------------" |
        \ let blkcoE="-------------------]]>"
    " LaTeX
    autocmd FileType tex
        \ let slco="%" |
        \ let blkcoS="\begin{comment}" |
        \ let blkcoE="\end{comment}"

    " All Code Files
    autocmd FileType java,c,c++,cpp,h,h++,hpp,xml
        \ vnoremap <silent> <S-Tab> :call BLKCOtoggle(1)<cr> |
        \ nnoremap <silent> <S-Tab> :call BLKCOtoggle(0)<cr>

    autocmd FileType java,c,c++,cpp,h,h++,hpp,sql,xml,sh,ksh,csh,tcsh,zsh,bash,dash,pl,python,vim,vimrc,ahk,tex,make,gdb
        \ nnoremap <silent> <Tab> :call SLCOtoggle()<cr>
    autocmd FileType sh,ksh,csh,tcsh,zsh,bash,pl,python,sql,vim,vimrc,ahk,tex,make,gdb
        \ nnoremap <silent> <S-Tab> :'k,.call SLCOtoggle()<cr>

    autocmd FileType java,c,c++,cpp,h,h++,hpp,sql,sh,ksh,csh,tcsh,zsh,bash,pl,vim,vimrc
        \ nnoremap <silent> todo oTODO: <esc><Tab>==A
    autocmd FileType java,c,c++,cpp,h,h++,hpp,sql,sh,ksh,csh,tcsh,zsh,bash,pl,vim,vimrc
        \ nnoremap <silent> fixme oFIXME: <esc><Tab>==A
augroup END
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

    " Disable syntax highlight for files larger than 50 MB
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Turn off diffing
    autocmd VimLeave * windo diffoff

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    autocmd BufRead,BufNewFile *.confluence set filetype=confluencewiki spell foldmethod=manual
augroup END
" }}}

" vim: set foldmethod=marker number relativenumber formatoptions-=tc:
