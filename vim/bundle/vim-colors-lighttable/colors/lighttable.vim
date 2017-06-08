" Initialize color scheme
let colors_name = "lighttable"

" Undecorate defaults
hi clear Normal
hi clear Comment
hi clear String
hi clear PreProc
hi clear Constant
hi clear Identifier
hi clear Statement
hi clear Type
hi clear Special
hi clear Underlined
hi clear Ignore
hi clear Error
hi clear Todo

" background: #202020; color:#ccc;
hi Normal ctermfg=252 ctermbg=234
" #9ac
hi Comment term=italic cterm=italic ctermfg=110
" #add
hi String ctermfg=152
" #aec
hi PreProc ctermfg=158
" #caf
hi Constant ctermfg=183
" Identifiers are explicitly nonstyled
hi Identifier term=NONE cterm=NONE ctermfg=252
" #aec for statement
hi Statement ctermfg=158
hi Type ctermfg=153
hi Special ctermfg=152
hi Underlined cterm=underline ctermfg=158

" Line numbers are like comments
hi LineNr ctermfg=243
hi CursorLineNr ctermfg=15 ctermbg=Black

" Use a subtle background change for the current line
hi Cursor ctermfg=White
hi CursorLine ctermbg=Black cterm=NONE
hi NonText ctermfg=243

" Some nice ruby customization
hi rubyInstanceVariable ctermfg=153
hi rubyFunction ctermfg=153
