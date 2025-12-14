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

(ert-deftest test-kai/show-shortcuts-with-adp ()
  "Test kai/show-shortcuts function includes ADP buttons when ADP devices are available."
  (cl-letf (((symbol-function 'kai/manson-powsup-get-devs) (lambda () nil))
            ((symbol-function 'kai/aim-tti-powsup-get-devs) (lambda () nil))
            ((symbol-function 'kai/adp-get-devs) (lambda () '("/dev/ttyACM0"))))
    (kai/show-shortcuts)
    (let ((buf (get-buffer "*kais shortcuts*")))
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match-p "Qualcomm ADP" content))
          (should (string-match-p "Power On" content))
          (should (string-match-p "Power Off" content))
          (should (string-match-p "Reboot" content))
          (should (string-match-p "Enter EDL" content))
          (should (string-match-p "Exit EDL" content)))))))

(ert-deftest test-kai/show-shortcuts-with-manson ()
  "Test kai/show-shortcuts function includes Manson power supply buttons when Manson devices are available."
  (cl-letf (((symbol-function 'kai/manson-powsup-get-devs) (lambda () '("/dev/ttyUSB0")))
            ((symbol-function 'kai/aim-tti-powsup-get-devs) (lambda () nil))
            ((symbol-function 'kai/adp-get-devs) (lambda () nil)))
    (kai/show-shortcuts)
    (let ((buf (get-buffer "*kais shortcuts*")))
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match-p "Manson Power Supply" content))
          (should (string-match-p "Power On" content))
          (should (string-match-p "Power Off" content))
          (should (string-match-p "Power Cycle" content))
          (should (string-match-p "Status" content)))))))

(ert-deftest test-kai/show-shortcuts-with-aim-tti ()
  "Test kai/show-shortcuts function includes AIM-TTi power supply buttons when AIM-TTi devices are available."
  (cl-letf (((symbol-function 'kai/manson-powsup-get-devs) (lambda () nil))
            ((symbol-function 'kai/aim-tti-powsup-get-devs) (lambda () '("/dev/ttyUSB1")))
            ((symbol-function 'kai/adp-get-devs) (lambda () nil)))
    (kai/show-shortcuts)
    (let ((buf (get-buffer "*kais shortcuts*")))
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match-p "AIM-TTi Power Supply" content))
          (should (string-match-p "Power On" content))
          (should (string-match-p "Power Off" content))
          (should (string-match-p "Power Cycle" content))
          (should (string-match-p "Status" content)))))))
