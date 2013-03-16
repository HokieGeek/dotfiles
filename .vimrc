""" Options
filetype plugin indent on

set nocompatible " Not compatible with plain vi
" set cindent
set autoindent
set smartindent
set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab
set history=500
set undolevels=500
set title
set noerrorbells
set visualbell
set ruler
set showcmd " Shows the command being typed
set wildmenu " Tab completion in command-line mode (:)
set wildignore=*.d,*.o,*.obj,*.bak,*.exe,*.swp,*~ " These file types are ignored when doing auto completions
set viminfo=%,'20,"100,<1000,s100,:150,n~/.viminfo " freaking awesome. RTFM
set incsearch " Searches as you type
set spelllang=en_us
set directory=/tmp
set browsedir=buffer " Open file browser in the current file's directory
set foldenable
set foldclose=all " Folds will close when the cursor leaves them
set backup
set showmatch
set list
set listchars=tab:>>,trail:.,extends:#,nbsp:.
set wrapscan " Searches wrap around the end of the file
" set splitbelow
set splitright
set diffopt+=iwhite " Vimdiff will ignore whitespace diffs
set ttyfast

if has("gui_running")
    colorscheme desert
    " colorscheme murphy
    set guioptions-=T " Never show the toolbar.
    set guioptions-=m " Never show the menubar.
else
    " colorscheme murphy
    colorscheme elflord
endif
" set background=dark

syntax on
" Disable syntax highlight for files larger than 50 MB
autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

""" Highlights
highlight CursorLine ctermbg=yellow ctermfg=black cterm=none
highlight SpecialKey ctermbg=black ctermfg=lightgrey cterm=none
" highlight NonText ctermbg=black ctermfg=lightgrey cterm=none
" highlight AFP ctermbg=darkblue ctermfg=red cterm=bold
highlight AFP ctermbg=grey ctermfg=white cterm=bold
match AFP /\cAFP/

""" Sessions
func! LoadSession()
    let session_file = $HOME."/.vim/sessions/".expand("%:t").".session"
    if filereadable(session_file)
        exe "source ".session_file
        syntax on
        redraw
        echo "Loaded saved session"
    endif
endfun
func! SaveSession(ifExists)
    let session_file = $HOME."/.vim/sessions/".expand("%:t").".session"
    if a:ifExists > 0 && filereadable(session_file) || a:ifExists == 0
        exe "mksession! ".session_file
        if filereadable(session_file)
            redraw
            echo "Saved session"
        else
            echoerr "Error saving session"
        endif
    endif
endfun
func! DeleteSession()
    let session_file = $HOME."/.vim/sessions/".expand("%:t").".session"
    if filereadable(session_file)
        call delete(session_file)
        echo "Deleted session"
    endif
endfun
au VimEnter * call LoadSession()
au VimLeavePre * call SaveSession(1)

""" Views
if ! &diff
    autocmd BufWinLeave * if expand("%") != "" | mkview | endif
    autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
endif

""" Commands
command! Dorig vnew | set bt=nofile | r # | 0d_ | windo diffthis
" command! Dgit vnew | set bt=nofile | r # | 0d_ | windo diffthis
" Will allow me to sudo a file that's open
cmap w!! %!sudo tee > /dev/null %

""" Keyboard mappings
nmap <Leader>13 maggg?G`a
nmap <Leader>s :source $MYVIMRC<CR>
nmap <Leader>v :tabnew $MYVIMRC<CR>
nmap <silent> <Leader><Leader> :nohlsearch<CR>
" Search down for a word under cursor in a new window
nmap <silent> <Leader>] :sp<CR>:res 15<CR>/<C-R><C-W><CR>
" Search up for a word under cursor in a new window
nmap <silent> <Leader>[ :sp<CR>:res 15<CR>?<C-R><C-W><CR>
nmap <silent> <F7> :call SaveSession(0)<CR>
nmap <silent> <Leader><F7> :call DeleteSession()<CR>
nmap <silent> <F8> :bnext<CR>
nmap <silent> <S-F8> :bprevious<CR>
nmap <silent> <F9> :set cursorline!<CR>:set number!<CR>
nmap <silent> <F10> :setlocal spell!<CR>
" <F11> is too often taken by the terminal's FULLSCREEN handler
nmap <silent> <F12> :if ! &diff<CR>Dorig<CR>else<CR>only<CR>diffoff<CR>endif<CR>
" nmap <silent> <S-F12> :if ! &diff<CR>Dgit<CR>else<CR>only<CR>diffoff<CR>endif<CR>

"" I feel like being a pain in the ass
noremap <Up> :echoerr "Use k instead!"<CR>
noremap <Down> :echoerr "Use j instead!"<CR>
noremap <Left> :echoerr "Use h instead!"<CR>
noremap <Right> :echoerr "Use l instead!"<CR>

"" Fixing a couple of my worst capitalization issues. I feel bad about this...
nmap K k
command! W w

""" Abbreviations
ab afp]] [AFP]

""" Commenting
func! SLCOtoggle()
    if getline(".") =~ '^\s*'.g:slco " Remove the comment tag it contains
        exe "silent normal ma:.s;".escape(g:slco, "[]")." *;;:nohlsearch`a".eval(strlen(g:slco)+1)."h"
    else " Comment line
        exe "normal maI".g:slco." `a".eval(strlen(g:slco)+1)."l"
    endif

    if exists("g:slcoE")
        if getline(".") =~ g:slcoE.'\s*$'  " Remove the end comment tag it contains
            exe "silent normal ma:.s; *".escape(g:slcoE, "[]").";;:nohlsearch`a3h"
        else " Comment line (end)
            exe "normal ma^A ".g:slcoE." `a"
        endif
    endif
endfun
func! BLKCOtoggle(isVisual)
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
            let mark="k"
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
endfun

if exists("g:slcoE")
    unlet! slcoE
endif
au FileType vim,vimrc let slco="\"" " vim
au FileType sql let slco="--"       " SQL
au FileType ahk let slco=";"        " AutoHotkey
au FileType tex let slco="%"        " LaTeX
au FileType tex let blkcoS="\begin{comment}"
au FileType tex let blkcoE="\end{comment}"
" Shell/Scripts
au FileType sh,ksh,csh,tcsh,zsh,bash,dash,pl,python,make,gdb let slco="#"
" XML
au FileType xml,html let slco="<![CDATA[-----"
au FileType xml,html let slcoE="-----]]>"
au FileType xml,html let blkcoS="<![CDATA[-------------------"
au FileType xml,html let blkcoE="-------------------]]>"
" Java/C/C++
au FileType java,c,c++,cpp,h,h++,hpp let slco="//"
au FileType java,c,c++,cpp,h,h++,hpp let blkcoS="/*"
au FileType java,c,c++,cpp,h,h++,hpp let blkcoE="*/"
" All Code Files
au FileType java,c,c++,cpp,h,h++,hpp,sql,xml,sh,ksh,csh,tcsh,zsh,bash,dash,pl,python,vim,vimrc,ahk,tex,make,gdb map <silent> <Tab> :call SLCOtoggle()<CR>
au FileType java,c,c++,cpp,h,h++,hpp,xml vmap <silent> <S-Tab> :call BLKCOtoggle(1)<CR>
au FileType java,c,c++,cpp,h,h++,hpp,xml nmap <silent> <S-Tab> :call BLKCOtoggle(0)<CR>
au FileType sh,ksh,csh,tcsh,zsh,bash,pl,python,sql,vim,vimrc,ahk,tex,make,gdb nmap <silent> <S-Tab> :'k,.call SLCOtoggle()<CR>
au FileType java,c,c++,cpp,h,h++,hpp,sql,sh,ksh,csh,tcsh,zsh,bash,pl,vim,vimrc map <silent> todo oTODO: <ESC><Tab>==A
au FileType java,c,c++,cpp,h,h++,hpp,sql,sh,ksh,csh,tcsh,zsh,bash,pl,vim,vimrc map <silent> fixme oFIXME: <ESC><Tab>==A

""" Syntax lookups / Tagging
" if filereadable("??/tags")
    " au FileType c,c++,cpp,h,h++,hpp set tags=./tags
    " au BufwinEnter *.* echo "Loaded tags file"

    " Map some keys to access these
    " nmap <silent> <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
" else
    " nmap <silent> <C-\> :echoerr "No tags file loaded"<CR>
" endif

"" Use <C-X><C-O> to access these
au FileType c,c++,cpp,h,h++,hpp set omnifunc=ccomplete#Complete
au FileType java set omnifunc=javacomplete#Complete
au FileType python set omnifunc=pythoncomplete#Complete
au FileType javascript set omnifunc=javascriptcomplete#CompleteJS
au FileType css set omnifunc=csscomplete#CompleteCSS
au FileType html set omnifunc=htmlcomplete#CompleteTags
au FileType php set omnifunc=pythoncomplete#CompletePHP
au FileType xml set omnifunc=xmlcomplete#CompleteTags

""" silly little time savers
" Wraps brackets around a single-line if statement body
au FileType c,c++,cpp,h,h++,hpp,java map <silent> <C-K> ^f(%A {wi<CR>o}kO

" I have no idea why this is here
" Steve Hall wrote this function
" See :help attr-list for possible attrs to pass
function! Highlight_remove_attr(attr)
    " save selection registers
    new
    silent! put

    " get current highlight configuration
    redir @x
    silent! highlight
    redir END
    " open temp buffer
    new
    " paste in
    silent! put x

    " convert to vim syntax (from Mkcolorscheme.vim,
    "   http://vim.sourceforge.net/scripts/script.php?script_id=85)
    " delete empty,"links" and "cleared" lines
    silent! g/^$\| links \| cleared/d
    " join any lines wrapped by the highlight command output
    silent! %s/\n \+/ /
    " remove the xxx's
    silent! %s/ xxx / /
    " add highlight commands
    silent! %s/^/highlight /
    " protect spaces in some font names
    silent! %s/font=\(.*\)/font='\1'/

    " substitute bold with "NONE"
    execute 'silent! %s/' . a:attr . '\([\w,]*\)/NONE\1/geI'
    " yank entire buffer
    normal ggVG
    " copy
    silent! normal "xy
    " run
    execute @x

    " remove temp buffer
    bwipeout!

    " restore selection registers
    silent! normal ggVGy
    bwipeout!
endfunction
" autocmd BufNewFile,BufRead * call Highlight_remove_attr("bold")

