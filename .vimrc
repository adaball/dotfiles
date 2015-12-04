execute pathogen#infect()
syntax on
filetype plugin indent on

"" tabs and whatnot
set backspace=2
set tabstop=2
set shiftwidth=2 

"" look and feel
colorscheme elflord

"" movement between panes, C-J|K|H|L
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l

"" NERDTree
map <C-N> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
