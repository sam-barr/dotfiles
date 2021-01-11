call plug#begin()
Plug 'cocopon/iceberg.vim'
Plug 'lervag/vimtex'
Plug 'rust-lang/rust.vim'
call plug#end()

source ~/.config/nvim/abbreviations.vim

" My mappings
nnoremap <Tab> <C-w>
nnoremap Y y$
nnoremap <C-b> :call ToggleBackground()<CR>
inoremap <C-b> <Esc>:call ToggleBackground()<CR>a

nnoremap <C-s> :call SpellcheckPrevious()<CR>
inoremap <C-s> <Esc>:call SpellcheckPrevious()<CR>a

" Saving me from myself
nnoremap Q <nop>
nnoremap q: <nop>

" create latex environment
inoremap <C-e> <Esc>"syiWcc\begin{<Esc>"spa}<Cr>\end{<Esc>"spa}<Esc>O

function! ToggleBackground()
    if &background == "dark"
        set background=light
    else
        set background=dark
    endif
endfunction

function! SpellcheckPrevious()
    " spellcheck the previous error, and go back to where you were originally
    " adjusting left or right accordingly if the line length changed
    let l:col = getcurpos()[2]
    let l:length1 = strwidth(getline('.'))
    try
        normal! ms[s1z=`s
    catch
        echohl WarningMsg
        echo "Spell checking not enabled"
        echohl None
        return
    endtry
    let l:length2 = strwidth(getline('.'))
    if l:length1 > l:length2 && l:col < l:length2
        execute "norm" (l:length1 - l:length2) . "h"
    elseif l:length2 > l:length1
        execute "norm" (l:length2 - l:length1) . "l"
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
autocmd FileType c,cpp set tabstop=8 shiftwidth=8
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

set statusline+=%#MyStatusFile#\ %f\        " name of file
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
