execute pathogen#infect()

set autoindent
set encoding=utf8
set number
syntax on
colorscheme pablo
inoremap jk <esc>
set colorcolumn=80

"set cursorline
"highlight CursorLine cterm=NONE ctermbg=darkred ctermfg=darkred guibg=darkred guifg=darkred

autocmd BufRead,BufNewFile *.py set tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd BufRead,BufNewFile *.js set tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd BufRead,BufNewFile *.css set tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd BufRead,BufNewFile *.html set tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd BufRead,BufNewFile *.htm set tabstop=4 expandtab shiftwidth=4 softtabstop=4
autocmd BufRead,BufNewFile *.sql set tabstop=2 expandtab shiftwidth=2 softtabstop=2

set ttimeoutlen=50
set laststatus=2
set splitbelow
set splitright
set t_Co=256

let g:airline_theme='badwolf'
let g:airline#extensions#hunks#enabled=0
let g:airline#extensions#branch#enabled=1

"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_always_populate_loc_list=0
"let g:syntastic_auto_loc_list=0
"let g:syntastic_check_on_open=1
"let g:syntastic_check_on_wq=0

"highlight ColorColumn ctermbg=23
highlight ColorColumn ctermbg=22
