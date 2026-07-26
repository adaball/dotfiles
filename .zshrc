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


# vim keybindings
bindkey -v

# compinit error fix
# https://github.com/zsh-users/zsh-completions/issues/680
ZSH_DISABLE_COMPFIX="true"

# backwards search
bindkey '^R' history-incremental-search-backward

# command editing ('v' and 'ctrl-x-e')
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line
bindkey '^x^e' edit-command-line

# allow commends in interactive shells
setopt interactivecomments

# aliases
alias ch=':>$HISTFILE && exec $SHELL'
alias convert='magick'
alias cp='cp -iv'
alias dps="docker ps --format '{{.Names}}\n{{.State}} ({{.Status}})\n{{.Ports}}\n{{.Image}}\n---'"
alias ffmpeg='ffmpeg -hide_banner'
alias ffprobe='ffprobe -hide_banner'
alias grep='grep --color=auto'
alias history='history -d 1'
alias hv='history | sort -r | vim -'
alias ll='ls -lah'
alias ls='ls --color=auto'
alias mv='mv -iv'
alias remove_quarantine='xattr -d com.apple.quarantine'
alias rm='rm -iv'
alias youtube-dl='yt-dlp'
alias zconf='vim ~/.zshrc'

# PATH
export PATH="${HOME}/bin:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"

# fzf
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude .git'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# ruby
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# PS1
autoload -U colors && colors
export PS1="%K{129}%n@✧♡(◕‿◕✿)%k %~ $ "
