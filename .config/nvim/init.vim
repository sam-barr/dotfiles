call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
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

set number relativenumber
syntax on
filetype plugin indent on
set tabstop=4 shiftwidth=4 expandtab autoindent
set backspace=indent,eol,start
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

" status line

" status line always present
set laststatus=2

set statusline=
set statusline+=%#NormalColor#%{(mode()=='n')?'\ \ Normal\ ':''}
set statusline+=%#CommandColor#%{(mode()=='c')?'\ \ Command\ ':''}
set statusline+=%#InsertColor#%{(mode()=='i')?'\ \ Insert\ ':''}
set statusline+=%#ReplaceColor#%{(mode()=='R')?'\ \ Replace\ ':''}
set statusline+=%#VisualColor#%{(mode()=='v')?'\ \ Visual\ ':''}
set statusline+=%#VisualColor#%{(mode()==\"\\<C-v\>\")?'\ \ Visual\ Block\ ':''}
set statusline+=%#VisualColor#%{(mode()=='V')?'\ \ Visual\ Line\ ':''}

set statusline+=%#StatusFile#\ %t\           " name of file
set statusline+=%#StatusType#%=\ %y\         " file type
set statusline+=%#StatusPercent#\ %3p%%\     " percent through file
set statusline+=%#StatusLoc#\ %{GetPos()}\   " Position in file

function GetPos()
    let l:pos = getcurpos()
    return l:pos[1] . ":" . l:pos[2]
endfunction

hi NormalColor guifg=#161821 guibg=#818596
hi CommandColor guifg=#161821 guibg=#818596
hi InsertColor guifg=#161821 guibg=#84A0C6 
hi ReplaceColor guifg=#161821 guibg=#E27878
hi VisualColor guifg=#161821 guibg=#E2A478

hi StatusFile guifg=#C6C8D1 guibg=#2E313F
hi StatusType guifg=#6B7089 guibg=#0F1117
hi StatusLoc guifg=#161821 guibg=#818596
hi StatusPercent guifg=#C6C8D1 guibg=#2E313F
