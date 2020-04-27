call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
Plug 'itchyny/lightline.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'vifm/vifm.vim'
call plug#end()

colorscheme iceberg
au ColorScheme * hi Normal ctermbg=None guibg=NONE
au ColorScheme * hi NonText ctermbg=None guibg=NONE
au ColorScheme * hi EndOfBuffer ctermbg=None guibg=NONE
au ColorScheme * hi LineNr ctermbg=None guibg=NONE

"function! s:transparent() abort
"  highlight Normal ctermbg=NONE guibg=NONE
"  highlight NonText ctermbg=NONE guibg=NONE
"  highlight EndOfBuffer ctermbg=NONE guibg=NONE
"  highlight Folded ctermbg=NONE guibg=NONE
"  highlight LineNr ctermbg=NONE guibg=NONE
"  highlight CursorLineNr ctermbg=NONE guibg=NONE
"  highlight SpecialKey ctermbg=NONE guibg=NONE
"  highlight ALEErrorSign ctermbg=NONE guibg=NONE
"  highlight ALEWarningSign ctermbg=NONE guibg=NONE
"  highlight GitGutterAdd ctermbg=NONE guibg=NONE
"  highlight GitGutterChange ctermbg=NONE guibg=NONE
"  highlight GitGutterChangeDelete ctermbg=NONE guibg=NONE
"  highlight GitGutterDelete ctermbg=NONE guibg=NONE
"endfunction

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

" lightline stuff
let g:lightline = { 'colorscheme': 'iceberg' }

" clipboard
set clipboard=unnamedplus
