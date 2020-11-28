#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

source $HOME/.bash_shortcuts
source $HOME/.config/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1

PS1='[\u@\h \w]$(__git_ps1 " (%s)")\$ '


# Quick source bashrc if adding aliases on the fly
alias sbr="source ~/.bashrc"

alias gp="git push"
alias cls="clear"
alias stow="stow --target=$HOME"

# Quick quit the terminal because I'm too lazy to type the entire thing
alias q="exit"

PATH=$HOME/Dev/Scripts:$PATH
export TEMPLATES="$HOME/Dev/Templates"
export PATH="$HOME/neovim/bin:$PATH"
export PATH="~/.emacs.d/bin:$PATH"

# Set bash to edit in vi mode
set -o vi

cmkdir()
{
	[ -z "$1" ] && echo "cmkdir: missing operand" && return

	mkdir $1
	cd $1
}

