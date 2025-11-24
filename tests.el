(require 'ert)
(add-to-list 'load-path (file-name-concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         "lisp"))
(require 'libkai)

;; Mock data for testing
(defvar kai/test-udev-info-data
  '(("/dev/ttyUSB0" . "Silicon Labs CP2102")
    ("/dev/ttyUSB1" . "Silicon Labs CP2102")
    ("/dev/ttyACM0" . "Microchip Technology Inc")
    ("/dev/ttyACM1" . "Other Vendor")))

;; Mock function for testing
(defun kai/test-udevadm-info (tty)
  "Mock version of kai/udevadm-info for testing."
  (or (cdr (assoc tty kai/test-udev-info-data))
      ""))

(ert-deftest test-kai/zip-both-ends ()
  "Test kai/zip-both-ends function."
  (should (equal (kai/zip-both-ends '(1 2 3 4 5))
                 '(1 5 2 4 3)))
  (should (equal (kai/zip-both-ends '(1))
                 '(1)))
  (should (equal (kai/zip-both-ends '())
                 nil))
  (should (equal (kai/zip-both-ends '(1 2))
                 '(1 2))))


(ert-deftest test-kai/find-serial-devices ()
  "Test kai/find-serial-devices function."
  (cl-letf (((symbol-function 'kai/udevadm-info) #'kai/test-udevadm-info)
            ((symbol-function 'kai/cached) (lambda (fun _) (apply fun nil)))
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyUSB0" "/dev/ttyUSB1"
                             "/dev/ttyACM0" "/dev/ttyACM1")))))
    (should (equal (kai/find-serial-devices "Silicon Labs")
                   '("/dev/ttyUSB0" "/dev/ttyUSB1")))
    (should (equal (kai/find-serial-devices "Microchip Technology Inc")
                   '("/dev/ttyACM0")))
    (should (equal (kai/find-serial-devices "NonExisting")
                   nil))))

(ert-deftest test-kai/manson-powsup-get-devs ()
  "Test kai/manson-powsup-get-devs function."
  (cl-letf (((symbol-function 'kai/udevadm-info) #'kai/test-udevadm-info)
            ((symbol-function 'kai/cached) (lambda (fun _) (apply fun nil)))
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyUSB0" "/dev/ttyUSB1")))))
    (should (equal (kai/manson-powsup-get-devs)
                   '("/dev/ttyUSB0" "/dev/ttyUSB1")))))

(ert-deftest test-kai/adp-get-devs ()
  "Test kai/adp-get-devs function."
  (cl-letf (((symbol-function 'kai/udevadm-info) #'kai/test-udevadm-info)
            ((symbol-function 'kai/cached) (lambda (fun _) (apply fun nil)))
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyACM0" "/dev/ttyACM1")))))
    (should (equal (kai/adp-get-devs)
                   '("/dev/ttyACM0")))))

(ert-deftest test-kai/cached ()
  "Test kai/cached function."
  (should (equal (kai/cached #'random 1) (kai/cached #'random 1)))
  (should-not (equal (kai/cached #'random 1)
                     (progn
                       (sit-for 1.2)
                       (kai/cached #'random 1)))))

(ert-deftest test-kai/buffer-get-n-last-line ()
  (with-temp-buffer
    (insert "First line\nSecond line\nThird line\n")
    (should (equal (kai/buffer-get-n-last-line 1)
                   "Third line"))
    (goto-char (point-min))
    (should (equal (kai/buffer-get-n-last-line 2)
                   "Second line"))))
