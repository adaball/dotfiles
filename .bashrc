alias ls='ls --color'
alias ll='ls -lah --group-directories-first'
alias mv='mv -v'
alias rm='rm -iv'
alias rw='curl -s -L -w %{url_effective} -o /dev/null https://en.wikipedia.org/wiki/Special:Random && echo'

export PS1='\[\e[1;34m\] \w\[\e[1;37m\] $ '
export PATH="$PATH:/home/Adam/bin"
export TERM=xterm-256color

