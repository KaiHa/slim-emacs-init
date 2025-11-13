#!/usr/bin/env bash
dir=$(dirname $(readlink -f $0))
cd ${dir}
emacs --no-init-file --batch --load=./tests.el --funcall=ert-run-tests-batch-and-exit
