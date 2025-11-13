(require 'ert)
(add-to-list 'load-path (file-name-concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         "lisp"))
(require 'libkai)

;; Mock data for testing
(defvar kai/test-udev-info-data
  '(("/dev/ttyUSB0" . "ID_VENDOR=Silicon_Labs\nID_SERIAL=12345")
    ("/dev/ttyUSB1" . "ID_VENDOR=Silicon_Labs\nID_SERIAL=67890")
    ("/dev/ttyACM0" . "ID_VENDOR=Microchip_Technology_Inc.\nID_SERIAL=11111")
    ("/dev/ttyACM1" . "ID_VENDOR=Other_Vendor\nID_SERIAL=22222")))

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
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyUSB0" "/dev/ttyUSB1"
                             "/dev/ttyACM0" "/dev/ttyACM1")))))
    (should (equal (kai/find-serial-devices "ID_VENDOR=Silicon_Labs" "ttyUSB[0-9]+")
                   '("/dev/ttyUSB0" "/dev/ttyUSB1")))
    (should (equal (kai/find-serial-devices "ID_VENDOR=Microchip_Technology_Inc." "ttyACM[0-9]+")
                   '("/dev/ttyACM0")))
    (should (equal (kai/find-serial-devices "ID_VENDOR=NonExisting" "tty.*")
                   nil))))

(ert-deftest test-kai/powsup-get-devs ()
  "Test kai/powsup-get-devs function."
  (cl-letf (((symbol-function 'kai/udevadm-info) #'kai/test-udevadm-info)
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyUSB0" "/dev/ttyUSB1")))))
    (should (equal (kai/powsup-get-devs)
                   '("/dev/ttyUSB0" "/dev/ttyUSB1")))))

(ert-deftest test-kai/adp-get-devs ()
  "Test kai/adp-get-devs function."
  (cl-letf (((symbol-function 'kai/udevadm-info) #'kai/test-udevadm-info)
            ((symbol-function 'directory-files)
             (lambda (_dir _full pattern)
               (seq-filter (lambda (f) (string-match-p pattern f))
                           '("/dev/ttyACM0" "/dev/ttyACM1")))))
    (should (equal (kai/adp-get-devs)
                   '("/dev/ttyACM0")))))
