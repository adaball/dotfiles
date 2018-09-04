execute pathogen#infect()
execute pathogen#helptags()
syntax on
filetype plugin indent on

if !empty(glob("~/.vim/colors/base16-brewer.vim"))
  colorscheme base16-brewer
endif

set backspace=2
set expandtab
set nu
set shiftwidth=2
set tabstop=2

au BufNewFile,BufRead *.localbashrc setlocal ft=sh

let vimfiles_dir = $HOME . "/.vim/"

if has('win32') || has('win64')
  let vimfiles_dir = $HOME . '\vimfiles\'
endif

let &undodir = vimfiles_dir . '.undo'
let &backupdir = vimfiles_dir . '.backup'
let &directory = vimfiles_dir . '.swap'
let &dir = &directory

if has('gui_running')
  set guifont=Consolas:h14
endif

" movement between panes, C-J|K|H|L
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k
nmap <C-H> <C-W>h
nmap <C-L> <C-W>l

" NERDTree
map <C-N> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

