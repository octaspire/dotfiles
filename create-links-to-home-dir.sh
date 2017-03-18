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

echo "Check that the path below is correct before continuing. That directory should contain the dotfiles to be linked\n$(dirname $0)/"
echo -n "Continue and really overwrite your (.gitconfig .tmux.conf .vimrc .nexrc .emacs .zshrc) dotfiles (y/n)? "
read answer
if echo "$answer" | grep -iq "^y" ;then

    ln -sf "$(dirname $0)/gitconfig" ~/.gitconfig
    ln -sf "$(dirname $0)/tmux.conf" ~/.tmux.conf
    ln -sf "$(dirname $0)/vimrc"     ~/.vimrc
    ln -sf "$(dirname $0)/emacs"     ~/.emacs
    ln -sf "$(dirname $0)/zshrc"     ~/.zshrc
    ln -sf "$(dirname $0)/nexrc"     ~/.nexrc

    echo "OK."

else

    echo "Links NOT created."
fi

