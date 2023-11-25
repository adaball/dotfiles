# startup zsh file order (https://zsh.sourceforge.io/Doc/Release/Files.html)
# note: If ZDOTDIR is unset, HOME is used instead.
#
# 1. Commands are first read from /etc/zshenv
# 2. Commands are then read from $ZDOTDIR/.zshenv. 
# 3. If the shell is a login shell, commands are read from /etc/zprofile and then $ZDOTDIR/.zprofile. 
# 4. Then, if the shell is interactive, commands are read from /etc/zshrc and then $ZDOTDIR/.zshrc. 
# 5. Finally, if the shell is a login shell, /etc/zlogin and $ZDOTDIR/.zlogin are read. 

EDITOR=vim
VISUAL=vim
export PS1='%B%n%b %~ $ '

# vim keybindings
bindkey -v

# backwards search
bindkey '^R' history-incremental-search-backward

# command editing ('v' and 'ctrl-x-e')
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
bindkey '^x^e' edit-command-line

# allow commends in interactive shells
setopt interactivecomments

# reasonable defaults
alias cp='cp -iv'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias mv='mv -iv'
alias rm='rm -iv'

alias ll='ls -lah'
alias history='history 1'

# useful aliases
alias hv='history | sort -r | vim -'
alias ia='which wayback &>/dev/null && wayback --ia'

# localized additions
if [ -f ~/.localzshrc ]
then
  source ~/.localzshrc
fi
