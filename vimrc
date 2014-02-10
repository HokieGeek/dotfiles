""" Options {{{
filetype plugin indent on

set nocompatible " Not compatible with plain vi
" set autoread " Automatically loads file that has changed
set autoindent " Indents when you insert
" set smartindent
set cindent " C-like indenting
set tabstop=4 " Tab = 4 spaces. Because I'm not a savage
set softtabstop=4
set shiftwidth=4
set expandtab
set title
set noerrorbells
set visualbell
set ruler
set showcmd " Shows the command being typed
set wildmenu " Tab completion in command-line mode (:)
set wildignore=*.d,*.o,*.obj,*.bak,*.exe,*.swp,*~ " These file types are ignored when doing auto completions
set viminfo=!,%,'20,"100,<1000,s100,:150,n~/.viminfo " Remembers stuff. RTFM
set history=1000
set undolevels=1000
set directory=/tmp " Location of the swap file
set browsedir=buffer " Open file browser in the current file's directory
set foldenable
set showmatch " Briefly jumps to matching bracket
set incsearch " Searches as you type
set smartcase " If search pattern uses upper case chars, make search case sensitive
set wrapscan " Searches wrap around the end of the file
set nowrap
set list " Displays unprintable characters (whitespace, essentially)
set listchars=tab:>>,trail:.,extends:#,nbsp:_ " Define what symbols to use with unprintable characters
set splitright " When doing a vertical split, it puts it to the right of the current window
set splitbelow " When doing a horizontal split, it puts it below the current window
set diffopt+=iwhite " Vimdiff will ignore whitespace diffs
set ttyfast " Smoother redrawing
set lazyredraw " Don't redraw during macros
set noscrollbind " Don't scroll windows synchronized
set dictionary=/usr/share/dict/words
set spellsuggest=best,10
set spelllang=en_us
set formatoptions+=n " Recognize numbered lists
set backup " Create a backup file
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*,/private/tmp/*
set writebackup " Make a backup before overwriting a file.
set hlsearch
if $USER != "root"
    set modelines=1
endif
if exists('$TMUX')
    set clipboard=
else
    set clipboard=unnamed "sync with OS clipboard
endif
set hidden " you can change buffers without saving
set timeoutlen=500 "Let's see if this works for me

if !empty($TMUX)
    set t_Co=256
endif
if has("gui_running")
    colorscheme solarized
    colorscheme badwolf
    set guioptions-=T " Never show the toolbar.
    set guioptions-=m " Never show the menubar.
else
    colorscheme desert
endif

" Turns on spell check when typing out long git commit messages
autocmd FileType gitcommit set spell
" Disable syntax highlight for files larger than 50 MB
autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif
autocmd VimLeave * windo diffoff
syntax on
" }}}

""" Highlights {{{
highlight CursorLine ctermbg=yellow ctermfg=black cterm=none
highlight SpecialKey ctermbg=black ctermfg=lightgrey cterm=none

highlight AFP ctermbg=darkblue ctermfg=red cterm=bold
let m = matchadd("AFP", "AFP")
let m = matchadd("AFP", "afp")

autocmd Filetype GitLog
    \ highlight CursorLine ctermbg=darkblue ctermfg=white cterm=bold |
    \ highlight GitLogTime ctermbg=none ctermfg=cyan cterm=none | let m = matchadd("GitLogTime", " \(.* ago\) \<") |
    \ highlight GitLogAuthor ctermbg=none ctermfg=green cterm=none | let m = matchadd("GitLogAuthor", "\<.*\> -") |
    \ highlight GitLogMessage ctermbg=none ctermfg=lightgray cterm=none | let m = matchadd("GitLogMessage", "-.*") |
    \ highlight GitLogBranch ctermbg=none ctermfg=yellow cterm=none | let m = matchadd("GitLogBranch", "- \(.*\)") |
    \ highlight GitLogGraph ctermbg=none ctermfg=lightgray cterm=none | let m = matchadd("GitLogGraph", "^[\*\s|/\]* ") |
    \ highlight GitLogDash ctermbg=none ctermfg=lightgray cterm=none | let m = matchadd("GitLogDash", "-")
" }}}

""" Notes options {{{
augroup Notes
    autocmd!

    autocmd BufRead,BufNewFile *
        \ if &filetype == "" |
        \   nmap <silent> <F5> :set filetype=notes<CR> |
        \ endif

    autocmd Filetype notes
        \ setlocal spell |
        \ highlight NotesHeader ctermbg=darkblue ctermfg=grey cterm=bold,underline | let m = matchadd("NotesHeader", ">> .* <<$") |
        \ highlight NotesSection1 ctermbg=black ctermfg=darkgreen cterm=none | let m = matchadd("NotesSection1", "^== .*$") |
        \ highlight NotesSection2 ctermbg=black ctermfg=darkcyan cterm=none | let m = matchadd("NotesSection2", " == .*$") |
        \ highlight NotesNoticeMe ctermbg=lightyellow ctermfg=black cterm=none | let m = matchadd("NotesNoticeMe", "_.*_") |
        \ highlight NotesActionItem ctermbg=darkmagenta ctermfg=lightgrey cterm=underline | let m = matchadd("NotesActionItem", "@ .*$") |
        \ highlight NotesPersonCallout ctermbg=black ctermfg=blue cterm=bold | let m = matchadd("NotesPersonCallout", "\\[.*\\]")
augroup END
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
    autocmd BufWinLeave * if expand("%") != "" | mkview! | endif
    autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
endif
" }}}

""" Commands {{{
func! LoadedContentClear()
    set modifiable
    bwipeout content
    diffoff
    silent loadview 9
    unlet! g:loaded_output
endfun
func! LoadContent(location, command)
    let g:loaded_output = 1
    if a:location == "left"
        topleft vnew
    elseif a:location == "right"
        botright vnew
    elseif a:location == "top"
        topleft new
    elseif a:location == "bottom"
        botright new
    endif
    set bt=nofile
    exe "silent r ".a:command
    exe "silent file content_".a:location
    0d_
endfun
func! PopDiff(command)
    if exists("g:loaded_output")
        call LoadedContentClear()
    endif

    mkview! 9
    call LoadContent("left", a:command)
    wincmd l
    windo set diffthis
    0
    set nomodifiable
endfun
func! PopGitDiffPrompt()
    if exists("g:loaded_output")
        call LoadedContentClear()
    endif

    call inputsave()
    let l:response = input('Commit, tag or branch: ')
    call inputrestore()
    call PopDiff("!git show ".l:response.":./#")
endfun
func! PopSynched(command)
    if exists("g:loaded_output")
        call LoadedContentClear()
    endif

    mkview! 9
    let l:cline = line(".")
    " let l:ft = &filetype
    set foldenable!
    0
    call LoadContent("left", a:command)
    " windo set filetype=l:ft
    windo set scrollbind nomodifiable
    exe l:cline
    set modifiable
endfun
func! PopGitLog()
    if exists("g:loaded_output")
        call LoadedContentClear()
    endif

    mkview! 9
    call LoadContent("top", "!git log --graph --pretty=format:'\\%h (\\%cr) <\\%an> -\\%d \\%s' #")
    set filetype=GitLog
    set nolist cursorline
    res 10
    set nomodifiable
    " wincmd p
endfun
func! PopGitDiffFromLog()
    let l:line = getline(".")
    call LoadedContentClear()
    call PopDiff("!git show echo '".l:line."' | cut -d '(' -f1 | awk '{ print $NF }'``:./#")
endfun

" Will allow me to sudo a file that is open without write permissions
cmap w!! %!sudo tee > /dev/null %
" }}}

""" Keyboard mappings {{{
nmap <Leader>s :source $MYVIMRC<CR>
nmap <silent> <Leader><Leader> :nohlsearch<CR>

nmap <silent> <F9> :call SaveSession()<CR>
nmap <silent> <Leader><F9> :windo call SaveSession()<CR>
nmap <silent> <F10> :call DeleteSession()<CR>
nmap <silent> <Leader><F10> :call LoadSession()<CR>

nmap <silent> g/c :sp<CR>:res 15<CR>/<C-R><C-W><CR>
nmap <silent> g/w :<c-u>vimgrep // % <bar> cw<left><left><left><left><left><left><left>
nmap <silent> g// :<c-u>noau vimgrep // ** <bar> cw<left><left><left><left><left><left><left><left>
nmap <silent> g/. :<c-u>noau vimgrep /<C-R><C-W>/ ** <bar> cw<CR>

nmap <silent> gb :bnext<CR>
nmap <silent> gB :bprevious<CR>

nmap <silent> con :setlocal number! relativenumber!<CR>
nmap <silent> coc :setlocal cursorline!<CR>
nmap <silent> cow :setlocal wrap!<CR>
nmap <silent> cos :setlocal spell!<CR>
nmap <silent> col :setlocal list!<CR>

" Toggle off a popped window
nmap <silent> Uu :call LoadedContentClear()<CR>
" Diff unsaved changes against file saved on disk
nmap <silent> Uo :call PopDiff("#")<CR>
" Diff current file with a given git revision
nmap <silent> Uc :call PopGitDiffPrompt()<CR>
" Diff current file against branch head
nmap <silent> Uh :call PopDiff("!git show HEAD:./#")<CR>
" Git blame on the right-side
nmap <silent> Ub :call PopSynched("!git blame --date=short #")<bar>wincmd p<bar>vertical res 40<bar>wincmd p<CR>
" Git log up top
nmap <silent> Ul :call PopGitLog()<CR>
" A scratch space. Kinda useless, I think
nmap <silent> Us :botright new<bar>set bt=nofile noswapfile modifiable<bar>res 10<CR>

" Oh, man
inoremap jk <esc>

augroup GitLog
    autocmd!
    " TODO: How to unload a map with the filetype no longer fits?
    autocmd Filetype GitLog nmap <silent> <enter> :call PopGitDiffFromLog()<CR>
    autocmd Filetype GitLog nmap <silent> <esc> :call LoadedContentClear()<CR>
augroup END

"" I feel like being a pain in the ass
noremap <Up> :echoerr "Use k instead! :-p"<CR>
noremap <Down> :echoerr "Use j instead! :-p"<CR>
noremap <Left> :echoerr "Use h instead! :-p"<CR>
noremap <Right> :echoerr "Use l instead! :-p"<CR>
" }}}

""" Commenting {{{
function! SLCOtoggle()
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
endfunction
function! BLKCOtoggle(isVisual)
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
endfunction

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
        \ vmap <silent> <S-Tab> :call BLKCOtoggle(1)<CR> |
        \ nmap <silent> <S-Tab> :call BLKCOtoggle(0)<CR>

    autocmd FileType java,c,c++,cpp,h,h++,hpp,sql,xml,sh,ksh,csh,tcsh,zsh,bash,dash,pl,python,vim,vimrc,ahk,tex,make,gdb
        \ map <silent> <Tab> :call SLCOtoggle()<CR>
    autocmd FileType sh,ksh,csh,tcsh,zsh,bash,pl,python,sql,vim,vimrc,ahk,tex,make,gdb
        \ nmap <silent> <S-Tab> :'k,.call SLCOtoggle()<CR>

    autocmd FileType java,c,c++,cpp,h,h++,hpp,sql,sh,ksh,csh,tcsh,zsh,bash,pl,vim,vimrc
        \ map <silent> todo oTODO: <ESC><Tab>==A|
        \ map <silent> fixme oFIXME: <ESC><Tab>==A
augroup END
" }}}

""" Completion {{{
" if filereadable("??/tags")
    " autocmd FileType c,c++,cpp,h,h++,hpp set tags=./tags
    " autocmd BufwinEnter *.* echo "Loaded tags file"

    " Map some keys to access these
    " nmap <silent> <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
" else
    " nmap <silent> <C-\> :echoerr "No tags file loaded"<CR>
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

" vim: set foldmethod=marker:
