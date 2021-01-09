export GPG_TTY=$(tty)

PS1='\A $(apm -l)% \w$ '         # HH:MM CHARGE% WORKING_DIR

alias wip='git commit -am WIP'
alias la='ls -lath'

OCTASPIRE_PASSDIR=~/.password-store
OCTASPIRE_PASSPATHS=$(find "$OCTASPIRE_PASSDIR" -path ./.git -prune -o -name '*.gpg' -print | sed "s~${OCTASPIRE_PASSDIR}/~~g" | sed 's~.gpg$~~g')
set -A complete_pass_2 -- $OCTASPIRE_PASSPATHS
