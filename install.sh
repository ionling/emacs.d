#!/bin/sh

mkdir -p ~/.emacs.d

cmd='readlink -f'
ln -is $($cmd init.el) ~/.emacs.d/init.el
ln -is $($cmd core) ~/.emacs.d/
ln -is $($cmd lisp) ~/.emacs.d/
ln -is $($cmd v) ~/.emacs.d/
ln -is $($cmd modules) ~/.emacs.d/
