execute pathogen#infect()
execute pathogen#helptags()
syntax on
filetype plugin indent on

set backspace=2
set expandtab
set hlsearch
set nu
set shiftwidth=2
set tabstop=2

au BufNewFile,BufRead *.localbashrc setlocal ft=sh
colorscheme elflord

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

" insert actual tab char when needed
inoremap <S-Tab> <C-V><Tab>

" NERDTree
map <C-N> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" Mouse
set mouse=a
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2
end

" pbcopy for macOS
if has('mac')
  map <F2> :.w !pbcopy<CR>
end

" run a shell command with its output in a new window
function! Command()
  call inputsave()
  let cmd = input('enter command: ')
  call inputrestore()

  execute "new | 0read ! " . cmd
endfunction
