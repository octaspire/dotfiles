function my_vi_bindings
    fish_vi_key_bindings
    bind -M insert -m default ö backward-char force-repaint
end

set -g fish_key_bindings my_vi_bindings

alias e="emacsclient -t"
alias wip="git commit -am WIP"

set PATH ~/bin $PATH

set -x GPG_TTY (tty)

