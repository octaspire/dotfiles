export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

alias wip='git ci -am WIP'
alias la='ls -lath'
alias e='emacsclient -nw'

# Eternal bash history (from stackoverflow)
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# From coderwall.com
function b64() {
    cat $1 | base64 | xclip -selection clipboard;
}

export GPG_TTY=$(tty)
export PATH=~/bin:~/.gem/ruby/2.3.0/bin:$PATH

