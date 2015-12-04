alias ls='ls --color'
alias ll='ls -lah --group-directories-first'
alias mv='mv -v'
alias rm='rm -iv'
alias gcc='/usr/bin/i686-w64-mingw32-gcc'
alias rw='curl -s -L -w %{url_effective} -o /dev/null https://en.wikipedia.org/wiki/Special:Random && echo'
alias yd='youtube-dl -f bestvideo+bestaudio'

export PS1='\[\e[1;34m\] \w\[\e[1;37m\] $ '
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
export PATH="$PATH:/usr/x86_64-w64-mingw32/sys-root/mingw/bin:/usr/x86_64-w64-mingw32/bin"
export PATH="$PATH:/home/Adam/bin"
export TERM=xterm-256color

