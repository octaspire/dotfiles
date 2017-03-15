#-----------------------------------------------------------------------------
#  Octaspire dotfiles - Various configuration files
#  Copyright 2017 www.octaspire.com
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#-----------------------------------------------------------------------------
# Path to oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git per-directory-history)

export PATH="$HOME/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/games:/usr/games"

source $ZSH/oh-my-zsh.sh

export EDITOR='vim'
export TERM='xterm-256color'

alias emacs="emacs -nw"
function quit() {
    clear
    exit
}

# This is from zshwiki.org/home/examples/zleiab
setopt extendedglob
typeset -Ag abbrevations
abbrevations=(
    "ocve" "octaspire_container_vector"
    "ocus" "octaspire_container_utf8_string"
    )
magic-abbrev-expand() {
    local MATCH
    LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
    LBUFFER+=${abbrevations[$MATCH]:-$MATCH}
    zle self-insert
}
no-magic-abbrev-expand() {
    LBUFFER+=' '
}
zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand
bindkey -M isearch " " self-insert
