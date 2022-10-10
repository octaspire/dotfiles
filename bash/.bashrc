export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

alias wip='git ci -am WIP'
alias la='ls -lath'
alias vlime_server='sbcl --load $HOME/.config/nvim/pack/minpac/start/vlime/lisp/start-vlime.lisp'

function b64() {
  case "$OSTYPE" in
    darwin*) base64 -i "$1" | pbcopy ;;
    *)       base64 -i "$1" | xclip -selection clipboard ;;
  esac
}

GPG_TTY=$(tty)
export GPG_TTY
export FZF_DEFAULT_COMMAND='rg --files'
export PATH=$PATH:$HOME/bin:$HOME/Library/Python/3.10/bin:$HOME/.gem/ruby/2.3.0/bin:$HOME/.roswell/bin
export VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"

