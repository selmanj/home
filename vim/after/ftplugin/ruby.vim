" Just eliminate any hanging spaces
autocmd BufWritePre * :%s/\s\+$//e
" Use spaces instead of tabs, with 2 spaces for indention
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set autoindent

set textwidth=80

" Line numbers are pretty rad
set number
