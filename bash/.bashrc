export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

alias wip='git ci -am WIP'
alias la='ls -lath'
alias e='emacsclient -nw'
alias em='emacs -nw'

# From coderwall.com
function b64() {
    cat $1 | base64 | xclip -selection clipboard;
}

export GPG_TTY=$(tty)
export PATH=~/bin:~/.gem/ruby/2.3.0/bin:$PATH

if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
  __GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
  source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi
