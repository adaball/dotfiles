EDITOR=vim
VISUAL=vim
shopt -s histappend
shopt -s checkwinsize

alias cp='cp -iv'
alias grep='grep --color=auto'
alias ll='ls -lah'
alias mv='mv -iv'
alias rm='rm -iv'

export PS1='\[\e[1;34m\] \w\[\e[1;37m\] $ '

if [ -f ~/.localbashrc ]
then
  source ~/.localbashrc
fi
