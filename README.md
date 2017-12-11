# Dotfiles

Various configuration files to be managed with GNU Stow.

These dotfiles are licensed under the Apache License, Version 2.0.
Some content (for example some Emacs packages included) have separate licenses.
Those files are included here as a convenience and do not belong under this license.

## Usage

### When `dotfiles` resides in `$HOME`

````sh
cd ~/dotfiles
stow bash
stow emacs
stow git
stow tmux
stow vi
stow vim
stow zsh
````

### When `dotfiles` resides in `$HOME/some/path`

````sh
cd ~/some/path/dotfiles
stow -t ~/ bash
stow -t ~/ emacs
stow -t ~/ git
stow -t ~/ tmux
stow -t ~/ vi
stow -t ~/ vim
stow -t ~/ zsh
````

