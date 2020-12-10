call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
Plug 'itchyny/lightline.vim'
Plug 'rust-lang/rust.vim'
call plug#end()

source ~/.config/nvim/abbreviations.vim

" My mappings
noremap <Tab> <C-W><C-W>
nnoremap Y y$
map <leader>s 1z=

" Saving me from myself
nnoremap Q <nop>
nnoremap q: <nop>

" Colors
set background=dark termguicolors
colorscheme iceberg
let g:lightline = { 'colorscheme': 'iceberg' }

set number relativenumber
syntax on
filetype plugin indent on
set tabstop=4 shiftwidth=4 expandtab autoindent
set backspace=indent,eol,start
set laststatus=2
set spelllang=en
set nohlsearch
set clipboard=unnamedplus
set conceallevel=2

" tex stuff
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=2
let g:vimtex_quickfix_open_on_warning=0
let g:vimtex_compiler_progname = 'nvr'

" rust stuff
let g:rustfmt_autosave = 1

" Some filetype autocmds
autocmd FileType gitcommit,html,xhtml,markdown,tex set spell
autocmd FileType c set tabstop=8 shiftwidth=8
autocmd FileType html,xhtml,xml set tabstop=2 shiftwidth=2
