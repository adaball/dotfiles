execute pathogen#infect()
execute pathogen#helptags()
syntax on
filetype plugin indent on

set backspace=indent,eol,start
set expandtab
set hlsearch
set nu
set shiftwidth=4
set tabstop=4

" set vimfiles directory
if has('win32') || has('win64')
  let vimfiles_dir = $HOME . '\vimfiles\'
else
  let vimfiles_dir = $HOME . "/.vim/"
endif

" put undo/backup/swap files under main vim directory
let &undodir = vimfiles_dir . '.undo'
let &backupdir = vimfiles_dir . '.backup'
let &directory = vimfiles_dir . '.swap'
let &dir = &directory

" needed for colorschemes in the terminal
set termguicolors

" set colorscheme and some other things for the gui
if has('gui_running')
  set guifont=Monaco:h16
  colorscheme base16-monokai

  set guicursor=n-v-c-i:block-Cursor
  set guicursor+=n-v-c-i:blinkon0
else
  colorscheme catppuccin_mocha
endif

" change <Leader>
let mapleader = ","

" disable highlight for searches
nmap <Leader>h :noh<CR>

" window management
nmap <Leader>q :quit<CR>
nmap <Leader>x :quit!<CR>
nmap <Leader>w :write<CR>

" redo
nmap <Leader>r :redo<CR>

" movement between panes, C-J|K|H|L
nmap <C-J> <C-W>j
nmap <C-K> <C-W>k
nmap <C-H> <C-W>h
nmap <C-L> <C-W>l

" insert actual tab char when needed
inoremap <S-Tab> <C-V><Tab>

" NERDTree
map <C-N> :NERDTreeToggle<CR>
map <Leader>f :NERDTreeFind<CR>
let NERDTreeShowHidden=1
let NERDTreeIgnore=['\.DS_Store$[[file]]']

" Mouse
set mouse=a
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2
end

" run a command, put output in new buffer
function! CommandIntoBuffer()
  call inputsave()
  let cmd = input('enter command: ')
  call inputrestore()

  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap

  execute "0read ! " . cmd

  setlocal nomodifiable
  1
endfunction
map <Leader>c :call CommandIntoBuffer()<CR>

" spell check for the current buffer
function! SpellCheck()
  setlocal spell spelllang=en_us
endfunction

" disable recording with q
map q <Nop>

" for reading particularly nasty JSON files
set maxmempattern=2000000

" NOTES:
" - s/\v<(.)(\w*)/\u\1\L\2/g - capitalize every word on a line
