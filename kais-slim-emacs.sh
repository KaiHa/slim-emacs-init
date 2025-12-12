#!/usr/bin/env bash
[[ -e ~/.emacs ]] && echo "[W] Found a ~/.emacs file, please be aware that it might interfere with this configuration."
dir=$(dirname $(readlink -f $0))
mkdir -p ~/.slim-emacsd
emacs --init-directory=~/.slim-emacsd --load=${dir}/init.el "$@"
