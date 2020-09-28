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

# These two are from https://github.com/akermu/emacs-libvterm
# for vterm in GNU Emacs.
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

export GPG_TTY=$(tty)
export PATH=~/bin:~/.gem/ruby/2.3.0/bin:$PATH

if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
  __GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
  source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi
