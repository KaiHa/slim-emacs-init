#!/usr/bin/env -S ./kais-slim-emacs.sh --load

;; Add the lisp directory to the load path
(add-to-list 'load-path (file-name-concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         "lisp"))

;; Load the library
(require 'libkai)

;; Test the function
(message "Testing kai/show-shortcuts function...")

;; Mock the device detection functions for testing
(defun kai/manson-powsup-get-devs ()
  '("/dev/ttyUSB0" "/dev/ttyUSB1"))

(defun kai/aim-tti-powsup-get-devs ()
  '("/dev/ttyACM0"))

(defun kai/adp-get-devs ()
  '("/dev/ttyACM1"))

;; Call the function
(kai/show-shortcuts)
