#!/usr/bin/env bash
dir=$(dirname $(readlink -f $0))
cd ${dir}
emacs --batch --init-directory=${dir} --load=${dir}/init.el --load=ert --load=./tests.el --funcall=ert-run-tests-batch-and-exit
