# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add scripts and cabal bin for ghc
PATH="$HOME/svn/Sentinel/scripts:$HOME/.cabal/bin/:$PATH"

alias e=emacs
alias amp="$HOME/svn/Ampersand/dist/build/ampersand/ampersand"
alias proto="$HOME/svn/Prototype/dist/build/prototype/prototype"
alias apps="cd $HOME/svn/Prototype/apps"
alias plat="pdflatex -halt-on-error"
