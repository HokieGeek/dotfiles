set nocompatible " Not compatible with plain vi

if !exists("g:dot_vim_dir")
    if isdirectory(expand("$HOME/vimfiles"))
        let g:dot_vim_dir = expand("$HOME/vimfiles")
    else
        let g:dot_vim_dir = expand("$HOME/.vim")
    endif
endif

""" Plugins {{{
filetype off
filetype plugin indent off

if has("win32unix") || has("win32") || has("win64") " Cygwin and windows can't handle it
    let g:que__vcs_section_enabled = 0
endif

let g:have_plugins = 0
if has("vim_starting")
    if has("packages") " If native package manager exists, use it
        let g:have_plugins = 1
    elseif isdirectory(expand(g:dot_vim_dir."/vim-pathogen"))
        " If no native package manager, load pathogen and use that instead
        let g:have_plugins = 1
        execute "set runtimepath+=".g:dot_vim_dir.",".g:dot_vim_dir."/vim-pathogen"
        silent! execute pathogen#infect()
    endif
endif

if g:have_plugins
    let g:syntastic_javascript_checkers = ['jslint']

    let g:Que__vcs_funcref = function("vit#statusline#get")

    " vim-go specific mappings
    autocmd FileType go nmap <leader>b <Plug>(go-build)
    autocmd FileType go nmap <leader>t <Plug>(go-test)
    autocmd FileType go nmap <leader>c <Plug>(go-coverage-toggle)
    autocmd FileType go nmap <leader>a <Plug>(go-alternate-edit)
    autocmd FileType go nmap <leader>m <Plug>(go-rename)
    autocmd FileType go nmap <leader>r <Plug>(go-run)
    autocmd FileType go nmap <leader>i <Plug>(go-info)
    " let g:go_metalinter_autosave = 1
    " let g:go_metalinter_autosave_enabled = ['golint']
    " let g:go_metalinter_enabled = ['golint']
    " let g:go_metalinter_autosave_enabled = ['vet', 'golint', 'errcheck']
    " let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
    " let g:go_list_autoclose = 0
    " let g:go_list_type = "quickfix"

    let g:undotree_WindowLayout = 2
endif

filetype plugin indent on
" }}}

""" Options {{{
set autoindent " Indents when you insert
set copyindent " Uses previous indentation when autoindenting
set tabstop=4 " How many spaces to display when it gets a tab
set softtabstop=4 " Tab = 4 spaces. Because I'm not a savage
set shiftwidth=4 " Number of spaces to use for each step of (auto)indenting
set expandtab " If you hit the tab key, it inserts spaces
set shiftround " USe multiple of shiftwidth when indenting with '>' and '<'
set smarttab " Use shiftwidth at left margin, instead of tabstops
set backspace=indent,eol,start " Make backspace actually work, because why not?
set title
set noerrorbells
set visualbell
set ruler
set showcmd " Shows the command being typed
set noshowmode " Don't show -- INSERT --
set complete-=i " Don't search includes because they are slow
set completeopt=longest,menuone,preview " Changing insert mode completion options
set wildmenu " Tab completion in command-line mode (:)
set wildignore=*.d,*.o,*.obj,*.bak,*.exe,*.swp,*~,tags,.hg,.git,*.pyc,*.orig " These file types are ignored when doing auto completions
set wildmode=list:longest,full
set viminfo=h,'50,"100,<10000,s1000 " Remembers stuff. RTFM
set history=1000
set undolevels=2000 " The number of undos which will be stored. This will affect mem footprint
if has("persistent_undo")
    set undofile " Persist an undo file so you can undo even after closing a file
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
execute "set showbreak=\u2937"
execute "set listchars=tab:\u21C0\u21C0,trail:\uB7,extends:\u22EF,nbsp:_"
" Define divider characters (like windows and folds)
execute "set fillchars=fold:\u2501,vert:\u2502"
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
set autoread " Reloads a file that has been changed externally
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
if has("win32") || has("win64") || has("win32unix")
    set shellslash
endif

" Always want it
set t_Co=256
set background=dark
if has("gui_running")
    let g:sierra_Midnight = 1
    try
        colorscheme ir_black
    catch /E185:/
        colorscheme murphy
    endtry
    " Never show the toolbar (T), menubar (m), right (rR), left (lL) and bottom (b) scrollbars
    set guioptions-=T guioptions-=m guioptions-=r guioptions-=R guioptions-=l guioptions-=L guioptions-=b
    set guioptions+=c " Use console dialogs instead of popup dialogs
else
    if exists("$SSH_CONNECTION")
        let g:scheme="ir_black"
    else
        let g:sierra_Pitch = 1
        let g:scheme="sierra"
    endif

    try
        execute "colorscheme ".g:scheme
    catch /E185:/
        colorscheme desert
    endtry
    unlet g:scheme
endif
syntax on
" }}}

""" Functions {{{
function! MyHighlights() " {{{
    " highlight CursorLine ctermbg=yellow ctermfg=black cterm=none
    highlight SpecialKey ctermbg=black ctermfg=lightgrey cterm=none
    highlight Folded ctermbg=black ctermfg=darkgrey cterm=none

    " I like being able to spot my comments quickly
    highlight AFP ctermbg=darkblue ctermfg=red cterm=bold
    try
        call matchadd("AFP", "\cAFP")
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
        let g:my_schemes = split(glob(expand(g:dot_vim_dir."/colors")."/*.vim"), '\n')
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
"" Some user stuff " {{{
nnoremap <silent> <F12> :call CycleColorScheme()<cr>
if has ("gui_running")
    nnoremap <silent> <leader><F12> :colorscheme ir_black<cr>
    nnoremap <silent> <C-F12> :colorscheme badwolf<cr>
else
    nnoremap <silent> <leader><F12> :colorscheme sierra<cr>
    nnoremap <silent> <C-F12> :colorscheme ir_black<cr>
endif

" A scratch space. Kinda useless, I think
nnoremap <silent> gh :Scratch<cr>
" Ctrl+W is a horrible window control whatsit
nnoremap <silent> gw <c-w>

cnoremap w!! w !sudo tee % >/dev/null
" }}}

"" Plugins " {{{
if g:have_plugins
    " Session saving
    nnoremap <silent> <F9> :call sessioner#save()<cr>
    nnoremap <silent> <leader><F9> :windo call sessioner#save()<cr>
    nnoremap <silent> <F10> :call sessioner#delete()<cr>
    nnoremap <silent> <leader><F10> :call sessioner#load()<cr>

    " Split the term
    nnoremap <silent> gsh :Split<cr>
    nnoremap <silent> gsv :Vsplit<cr>
    nnoremap <silent> gR :Run<cr>

    " Other random ones
    nnoremap <silent> ga :ArgsToggle<cr>
    nnoremap <silent> go :TlistToggle<cr>

    " Search current file
    nnoremap <silent> \\ :Grep<space>
    " Search all open buffers
    nnoremap <silent> g\ :Grep -b<space>
    " Search all files in current directory and down
    nnoremap <silent> g/ :Grep -a<space>

    vmap <Enter> <Plug>(EasyAlign)
    nmap ge <Plug>(EasyAlign)

    nnoremap <silent> gc :UndotreeToggle<cr>:UndotreeFocus<cr>
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
    nnoremap <silent> coN :setlocal relativenumber!<cr>
    nnoremap <silent> con :setlocal number!<bar>if exists("&relativenumber")<bar>setlocal relativenumber!<bar>endif<cr>
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
nnoremap <silent> coh :setlocal hlsearch!<cr>
nnoremap <silent> coH :nohlsearch<cr>
nnoremap <silent> cop :setlocal paste!<cr>
nnoremap <silent> coF :setlocal foldenable!<cr>
nnoremap <silent> cof   :setlocal foldenable foldmethod=syntax<bar>echomsg &foldmethod<cr>
nnoremap <silent> coff  :setlocal foldenable foldmethod=manual<bar>echomsg &foldmethod<cr>
nnoremap <silent> cofff :setlocal foldenable foldmethod=marker<bar>echomsg &foldmethod<cr>
" }}}

"" Some (probably questionable) overrides/shortcuts " {{{
inoremap jk <esc>
inoremap kj <esc>

noremap j gj
noremap k gk
noremap gj j
noremap gk k

nnoremap ZZ :xa<cr>
nnoremap ZQ :qa!<cr>

nnoremap Y y$
nnoremap n nzz

nnoremap [[ [[z<CR>
nnoremap ]] ]]z<CR>

nnoremap gr :!%:p<cr><cr>
" }}}

"" I feel like being a pain in the ass " {{{
noremap <left> :echoerr "Use h instead! :-p"<cr>
noremap <right> :echoerr "Use l instead! :-p"<cr>
noremap <up> :echoerr "Use k instead! :-p"<cr>
noremap <down> :echoerr "Use j instead! :-p"<cr>
" }}}
" }}}

""" Misc {{{
augroup FiletypeOptions
    autocmd!

    autocmd BufNewFile,BufRead *.confluence set filetype=confluencewiki
    autocmd FileType confluencewiki setlocal wrap linebreak nolist spell foldmethod=manual
    autocmd FileType markdown setlocal spell
    autocmd FileType gitcommit setlocal spell

    autocmd BufNewFile,BufRead *.conkyrc set filetype=conkyrc
    autocmd BufNewFile,BufRead *.gradle set filetype=groovy
    autocmd BufNewFile,BufRead SConstruct set filetype=python
    autocmd BufNewFile,BufRead *.pkgbuild set filetype=sh
    autocmd BufNewFile,BufRead *.sc set filetype=scala
    autocmd BufNewFile,BufRead .bowerrc set filetype=json

    autocmd FileType make setlocal noexpandtab nolist
    autocmd FileType qf setlocal number | if exists("&relativenumber") | setlocal norelativenumber | endif
    autocmd FileType cpp setlocal foldmethod=syntax

    autocmd FileType go set nolist makeprg=go\ build

    autocmd FileType Vit* let w:que__disabled=1 | call QueDisableStatusLine()
augroup END

augroup HighlightingOptions
    autocmd!

    autocmd VimEnter,ColorScheme * call MyHighlights()

    " Disable syntax highlight for files larger than 50 MB (taken from vim tips site)
    autocmd BufWinEnter * if line2byte(line("$") + 1) > 50000000 | syntax clear | endif

    " Disable cursorline when in insert mode cause I don't really need that
    autocmd InsertEnter * let b:last_cursorline=&cursorline | set nocursorline
    autocmd InsertLeave * execute "let &cursorline=".b:last_cursorline." | unlet! b:last_cursorline"
augroup END

augroup MiscOptions
    autocmd!

    "" Stop asking about simultaneous edits.
    "" Copied from Damian Conway's lecture "More Instantly Better Vim" at OSCON 2013
    autocmd SwapExists * let v:swapchoice = 'o' |
                       \ echohl WarningMsg |
                       \ echomsg 'Duplicate edit session (readonly)' |
                       \ echohl None |

    " Automatically reload this file
    autocmd BufWritePost $MYVIMRC source $MYVIMRC

    " Stole this straight out of tpope/vim-eunuch (and modified it a bit)
    autocmd BufNewFile * let b:brand_new_file = 1
    autocmd BufWritePost,FileWritePost *
        \ if exists('b:brand_new_file') |
        \   if getline(1) =~? '^#!' && executable('chmod') == 1 |
        \       silent! execute '!chmod +x "<afile>"' |
        \       edit |
        \       filetype detect |
        \   endif |
        \   unlet! b:brand_new_file |
        \ endif
augroup END
" }}}

" vim: set foldmarker={{{,}}} foldmethod=marker formatoptions-=tc:
