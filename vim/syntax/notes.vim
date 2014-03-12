if exists("b:current_syntax")
    finish
endif

" syntax match NotesSection1 "^== .*$"
" syntax match NotesHeader ">> .* <<$"
" syntax match NotesSection2 " == .*$"
" syntax match NotesNoticeMe "_.*_"
" syntax match NotesActionItem "@ .*$"
" syntax match NotesPersonCallout "\\[.*\\]"

" highlight NotesSection1         ctermbg=black ctermfg=darkgreen cterm=none
" highlight NotesHeader           ctermbg=darkblue ctermfg=grey cterm=bold,underline
" highlight NotesSection2         ctermbg=black ctermfg=darkcyan cterm=none
" highlight NotesNoticeMe         ctermbg=lightyellow ctermfg=black cterm=none
" highlight NotesActionItem       ctermbg=darkmagenta ctermfg=lightgrey cterm=underline
" highlight NotesPersonCallout    ctermbg=black ctermfg=blue cterm=bold

let b:current_syntax = 'notes'
