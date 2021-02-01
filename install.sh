#!/bin/sh

mkdir -p ~/.emacs.d

files=(init.el better-prog-theme.el core lisp site-lisp v modules)

for f in ${files[@]}; do
    if test $(realpath $f) != $(realpath ~/.emacs.d/$f); then
        ln -si `realpath $f` ~/.emacs.d/
    fi
done
