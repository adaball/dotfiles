execute pathogen#infect()
syntax on
filetype plugin indent on

" tabs and whatnot
set backspace=2
set tabstop=2
set shiftwidth=2
set expandtab

" look and feel
colorscheme elflord
set nu

" movement between panes, C-J|K|H|L
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l

" <Leader> key re-bind
let mapleader=","

" NERDTree
map <C-N> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" RSpec and Dispatch
nnoremap <Leader>t :call RunCurrentSpecFile()<CR>
nnoremap <Leader>s :call RunNearestSpec()<CR>
nnoremap <Leader>l :call RunLastSpec()<CR>
nnoremap <Leader>a :call RunAllSpecs()<CR>

let g:rspec_command = "Dispatch rspec {spec}"
