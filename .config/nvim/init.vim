call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
call plug#end()

source ~/.config/nvim/abbreviations.vim

" My mappings
noremap <Tab> <C-W><C-W>
nnoremap Y y$
nnoremap <leader>b :call ToggleBackground()<CR>
nnoremap <leader>s 1z=

" Saving me from myself
nnoremap Q <nop>
nnoremap q: <nop>

function! ToggleBackground()
    if &background == "dark"
        set background=light
    else
        set background=dark
    endif
endfunction

" Colors
set background=dark
set termguicolors
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
set cursorline
set colorcolumn=90

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
set statusline+=%#MyNormalColor#%{get(normal,mode(),'')}
set statusline+=%#MyCommandColor#%{get(command,mode(),'')}
set statusline+=%#MyInsertColor#%{get(insert,mode(),'')}
set statusline+=%#MyReplaceColor#%{get(replace,mode(),'')}
set statusline+=%#MyVisualColor#%{get(visual,mode(),'')}

set statusline+=%#MyStatusFile#\ %t\        " name of file
set statusline+=%#MyStatusType#%=\ %r%m%y\  " read only/modified/filetype
set statusline+=%#MyStatusPercent#\ %3p%%\  " percent through file
set statusline+=%#MyStatusLoc#\ %l:%v\      " Position in file

hi MyNormalColor guifg=#161821 guibg=#818596
hi MyCommandColor guifg=#161821 guibg=#818596
hi MyInsertColor guifg=#161821 guibg=#84A0C6 
hi MyReplaceColor guifg=#161821 guibg=#E27878
hi MyVisualColor guifg=#161821 guibg=#E2A478

hi MyStatusFile guifg=#C6C8D1 guibg=#2E313F
hi MyStatusType guifg=#6B7089 guibg=#0F1117
hi MyStatusLoc guifg=#161821 guibg=#818596
hi MyStatusPercent guifg=#C6C8D1 guibg=#2E313F

let normal  = {'n': "  Normal  "}
let command = {'c': "  Command "}
let insert  = {"i": "  Insert  "}
let replace = {"R": "  Replace "}
let visual  = {"v": "  Visual  ",
        \ "\<C-v>": "  V-Block ",
        \      "V": "  V-Line  "}
