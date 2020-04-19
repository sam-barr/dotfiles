call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
Plug 'itchyny/lightline.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
call plug#end()

colorscheme iceberg

set number relativenumber
syntax on
filetype plugin indent on
filetype on
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
set backspace=indent,eol,start
set laststatus=2
set spelllang=en
set nohlsearch

" haskell stuff
let g:haskell_classic_highlighting = 1

"Latex stuff
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:tex_conceal='abdmg'
let g:vimtex_compiler_progname = 'nvr'
set conceallevel=1
autocmd FileType tex set spell

" rust stuff
let g:rustfmt_autosave = 1
