#!/bin/sh

if [ -f $HOME/.xprofile ]; then
    . $HOME/.xprofile
fi

if [ -f $HOME/.Xmodmap ]; then
    /usr/bin/xmodmap $HOME/.Xmodmap
fi

