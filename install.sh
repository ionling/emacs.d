#!/usr/bin/env bash

command -v realpath > /dev/null || {
    # https://stackoverflow.com/a/3572105/7134763
    realpath() {
        [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
    }
}


mkdir -p ~/.emacs.d

files=(init.el better-prog-theme.el core lisp site-lisp v modules)

for f in ${files[@]}; do
    if test $(realpath $f) != $(realpath ~/.emacs.d/$f); then
        ln -si `realpath $f` ~/.emacs.d/
    fi
done
