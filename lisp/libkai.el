;;; -*- lexical-binding: t -*-

(provide 'libkai)
(defun kai/udevadm-info (tty)
  (shell-command-to-string (format "udevadm info --name=%s" tty)))

(defun kai/read-tty-path (defaults)
  (if (and defaults (= (length defaults) 1))
      (car defaults)
    (read-file-name "TTY device: " "/dev/" defaults t)))

(defun kai/find-serial-devices (udev-search-string dev-pattern)
  "Find all serial devices."
  (cl-flet ((predicate (tty)
              (string-search udev-search-string (kai/udevadm-info tty))))
    (sort
     (seq-filter #'predicate (directory-files "/dev" t dev-pattern))
     :lessp #'string-version-lessp)))

(defun kai/zip-both-ends (l)
  "Zip the head and the tail of the list L such that '(0 1 2 3 4 5 6) becomes '(0 6 1 5 2 4 3)."
  (when l
    (cons (car l)
          (kai/zip-both-ends (reverse (cdr l))))))

(defun kai/serial-term (tty)
  (if-let* ((buf (get-buffer tty))
            (_proc (get-buffer-process buf)))
      (pop-to-buffer buf)
    (serial-term tty 115200)))


(defun kai/serial-open-all ()
  "Open all serial ports in a new frame."
  (interactive)
  (let* ((flist (append '((lambda () (select-frame (make-frame-command)))
                          (lambda () (select-window (split-window-right))))
                        (apply #'append
                               (make-list
                                7
                                '((lambda () (other-window -1) (select-window (split-window-below)))
                                  (lambda () (other-window 1) (split-window-below))))))))
    (mapcar
     (lambda (dev)
       (apply (pop flist) nil)
       (if-let* ((buf (get-buffer dev)))
           (display-buffer-same-window buf nil)
         (serial-term dev 115200))
       (balance-windows))
     (kai/zip-both-ends (mapcar #'car (kai/list-serial-ports))))))


(defun kai/adp-get-devs ()
  "Find all ADP devices."
  (kai/find-serial-devices "ID_VENDOR=Microchip_Technology_Inc." "ttyACM[0-9]+"))

(defun kai/adp-instruct (tty instruction)
  "Send INSTRUCTION to device at TTY."
  (unless (and (get-buffer tty)
               (get-buffer-process tty))
    (serial-term tty 115200)
    (bury-buffer))
  (with-current-buffer tty
    (term-send-string nil instruction)
    (term-send-input)))

(defun kai/adp-enter-edl (tty)
  "Enter Emergency Download Mode for device at TTY."
  (interactive (list (kai/read-tty-path (kai/adp-get-devs))))
  (kai/adp-instruct tty "PWR_OFF 1")
  (run-at-time 1 nil #'kai/adp-instruct tty "MD_EDL 1")
  (run-at-time 2 nil #'kai/adp-instruct tty "PWR_OFF 0"))

(defun kai/adp-exit-edl (tty)
  "Exit Emergency Download Mode for device at TTY."
  (interactive (list (kai/read-tty-path (kai/adp-get-devs))))
  (kai/adp-instruct tty "PWR_OFF 1")
  (run-at-time 1 nil #'kai/adp-instruct tty "MD_EDL 0")
  (run-at-time 2 nil #'kai/adp-instruct tty "PWR_OFF 0"))

(defun kai/adp-pwr-on (tty)
  "Power on device at TTY."
  (interactive (list (kai/read-tty-path (kai/adp-get-devs))))
  (kai/adp-instruct tty "PWR_OFF 0"))

(defun kai/adp-pwr-off (tty)
  "Power off device at TTY."
  (interactive (list (kai/read-tty-path (kai/adp-get-devs))))
  (kai/adp-instruct tty "PWR_OFF 1"))

(defun kai/adp-reboot (tty)
  "Reboot device at TTY."
  (interactive (list (kai/read-tty-path (kai/adp-get-devs))))
  (kai/adp-instruct tty "PWR_OFF 1")
  (run-at-time 2 nil #'kai/adp-instruct tty "PWR_OFF 0"))


(defun kai/powsup-get-devs ()
  "Find all Silicon Labs USB device in /dev."
  (kai/find-serial-devices "ID_VENDOR=Silicon_Labs" "ttyUSB[0-9]+"))

(defun kai/powsup-add-process-filter (tty)
  (cl-flet ((filterfunc (proc s)
              (with-current-buffer (get-buffer-create "*powsup replies*")
                (goto-char (point-max))
                (insert s)
                (if (and (>= (point-max) 3)
                         (string-equal
                          (buffer-substring (- (point-max) 3) (point-max))
                          "OK"))
                    (insert "\n")))))
    (add-function
     :before
     (process-filter (get-buffer-process tty)) #'filterfunc)))

(defun kai/powsup-instruct (tty instruction)
  "Send INSTRUCTION to power supply at TTY."
  (unless (and (get-buffer tty)
               (get-buffer-process tty))
    (serial-term tty 9600)
    (kai/powsup-add-process-filter tty)
    (bury-buffer))
  (with-current-buffer (get-buffer-create "*powsup replies*")
    (goto-char (point-max))
    (insert "INSTR: " instruction "\n"))
  (with-current-buffer tty
    (term-send-string nil (format "%s\r" instruction))))

(defun kai/powsup-on (tty)
  "Power on device at TTY."
  (interactive (list (kai/read-tty-path (kai/powsup-get-devs))))
  (kai/powsup-instruct tty "SOUT0"))

(defun kai/powsup-off (tty)
  "Power off device at TTY."
  (interactive (list (kai/read-tty-path (kai/powsup-get-devs))))
  (kai/powsup-instruct tty "SOUT1"))

(defun kai/powsup-powercycle (tty)
  "Power cycle device at TTY."
  (interactive (list (kai/read-tty-path (kai/powsup-get-devs))))
  (kai/powsup-off tty)
  (run-at-time 2 nil #'kai/powsup-on tty))

(defun kai/buffer-get-n-last-line (n)
  "Return the N last line as a string."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- 0 n))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun kai/powsup-status (tty)
  "Get status of power supply at TTY."
  (interactive
   (list (read-file-name "Power Supply TTY: " "/dev/" "ttyUSB0")))
  (kai/powsup-instruct tty "GETD")
  (with-timeout (0.5 "*timeout*")
    (with-current-buffer (get-buffer-create "*powsup replies*")
      (while (not (and (string-match-p "INSTR: GETD"
                                       (kai/buffer-get-n-last-line 2))
                       (string-match-p "OK$"
                                       (kai/buffer-get-n-last-line 1))))
        (sit-for 0.1))
      (apply #'format "ACT: %s.%sV %s.%sA"
             (seq-subseq
              (seq-partition (kai/buffer-get-n-last-line 1) 2)
              0 4)))))

(defun kai/serial-term-buffer-p ()
  (if-let* ((proc (get-buffer-process (current-buffer))))
      (eq (process-type proc) 'serial)
    nil))

(defun kai/send-file-over-serial-to-qnx (fname)
  "Send binary files over serial line to a QNX system.  Note that uudecode
is needed on the QNX side.  Also this function shall be called from the
serial-connection wich has the QNX shell open."
  (interactive "fSelect file to send: " term-mode)
  (if (kai/serial-term-buffer-p)
      (progn
        (term-send-string nil "cat > /tmp/incoming-base64-data")
        (term-send-input)
        (term-send-string
         nil
         (with-temp-buffer
           (insert-file-contents fname)
           (base64-encode-region (point-min) (point-max))
           (buffer-string)))
        (term-send-input)
        (term-send-string
         nil
         (format "base64 -d /tmp/incoming-base64-data > /tmp/%s && rm /tmp/incoming-base64-data"
                 (file-name-nondirectory fname)))
        (term-send-input)
        (message "File was send to the /tmp/ directory of the serial device."))
    (message "This function is meant to be called from a serial-term buffer!")))

(defun kai/list-serial-ports ()
  "Returns a LIST of CONS of serial-ports and description."
  (if (eq system-type 'windows-nt)
      (with-temp-buffer
        (call-process "mode" nil t)
        (mapcan
         (lambda (l)
           (if (string-prefix-p "Status for device COM" l)
               (list (cons
                      (format "\\\\.\\%s" (substring l 18 -1))
                      (substring l 18 -1)))))
         (string-lines (buffer-string) t)))
    (mapcar
     (lambda (tty)
       (cons
        tty
        (format "%s — %s"
                (file-name-base tty)
                (replace-regexp-in-string
                 "_"
                 " "
                 (replace-regexp-in-string
                  "\\(.*\n\\)*.*ID_SERIAL=\\(.*\\)\\(.*\n\\)*"
                  "\\2"
                  (kai/udevadm-info tty))))))
     (sort (directory-files "/dev" t "ttyUSB[0-9]+") :lessp 'string-version-lessp))))
