EDITOR=vim
VISUAL=vim
HISTCONTROL=ignoredups:erasedups
HISTSIZE=-1
shopt -s histappend
shopt -s checkwinsize

alias cp='cp -iv'
alias ll='ls -lah'
alias mv='mv -iv'
alias rm='rm -iv'

export PS1='\[\e[1;34m\] \w\[\e[1;37m\] $ '

if [ -f ~/.localbashrc ]
then
  source ~/.localbashrc
fi
