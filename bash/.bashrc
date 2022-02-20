[[ $PS1 && -f /usr/local/share/bash-completion/bash_completion.sh ]] && \
        source /usr/local/share/bash-completion/bash_completion.sh

alias wip='git ci -am WIP'
alias la='ls -lath'
alias e='emacsclient -nw'
alias em='emacs -nw'

export GPG_TTY=$(tty)

# This is for vterm in GNU Emacs.
[ -f "$HOME/.bashrc_vterm" ] && . "$HOME/.bashrc_vterm"

PS1='\e[0;31m\t \h:\w\e[m\n\$ '

eval `ssh-agent -s`
