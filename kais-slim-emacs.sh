#!/usr/bin/env bash
[[ -e ~/.emacs ]] && echo "[W] Found a ~/.emacs file, please be aware that it might interfere with this configuration."
dir=$(dirname $(readlink -f $0))
emacs --init-directory=${dir} --load=${dir}/init.el "$@"
