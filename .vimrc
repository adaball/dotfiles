execute pathogen#infect()
syntax on
filetype plugin indent on

colorscheme chroma

set backspace=2
set directory=~/.vim/swap//
set expandtab
set nu
set shiftwidth=2
set tabstop=2

" movement between panes, C-J|K|H|L
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l

" NERDTree
map <C-N> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

