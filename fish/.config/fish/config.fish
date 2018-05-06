alias e="emacsclient -t"
alias em="emacs -nw"
alias wip="git commit -am WIP"

set PATH ~/bin $PATH

set -x GPG_TTY (tty)

switch (uname)
       case Darwin
       # Add coreutils (for example sha512sum) into the PATH and MANPATH.
       # Coreutils can be installed on macOS with
       # brew install coreutils
       set -x PATH /usr/local/opt/coreutils/libexec/gnubin $PATH
       set -x MANPATH /usr/local/opt/coreutils/libexec/gnuman $MANPATH
end

# Load private settings from a separate file, if it is present.
if test -e  ~/.secrets/config/fish/config.fish
   source ~/.secrets/config/fish/config.fish
end
