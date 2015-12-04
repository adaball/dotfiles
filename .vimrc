syntax on
set bs=2

"" map <F5> :w \| :! ./% <CR>
colorscheme elflord

autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
autocmd Filetype ruby map <F5> :w \| :! ruby % <CR>
