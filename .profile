# Ensure tmux and others who use interactive login shells actually load .bashrc.
# The reasoning is a bit confusing, but is detailed here:
#
# https://unix.stackexchange.com/a/541352
if [ -n "$BASH_VERSION" -a -n "$PS1" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
    fi
fi

