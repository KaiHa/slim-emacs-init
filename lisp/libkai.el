;;; -*- lexical-binding: t -*-

(provide 'libkai)

(defgroup kai/serial nil "Additional serial settings.")

(defcustom kai/serial-ignored-devices
  nil
  "List of paths to serial devices that should be ignored in the serial term menu."
  :type '(repeat string)
  :group 'kai/serial)

(defvar libkai--cache (make-hash-table :test 'eq)
  "Hash table to store cached values.")

(defun kai/cached (fun timeout)
  "Call FUN, caching the result for TIMEOUT seconds.
If the cache is valid, return the cached value; otherwise, recompute."
  (let ((cached (gethash fun libkai--cache)))
    (if (and cached
             (time-less-p nil (time-add (cdr cached) timeout)))
        (car cached)
      (let ((result (apply fun nil)))
        (puthash fun (cons result (current-time)) libkai--cache)
        result))))

(defun kai/udevadm-info (tty)
  (shell-command-to-string (format "udevadm info --name=%s" tty)))

(defun kai/read-tty-path (defaults)
  (if (and defaults (= (length defaults) 1))
      (car defaults)
    (read-file-name "TTY device: " "/dev/" defaults t)))

(defun kai/find-serial-devices (regex)
  "Find all serial devices."
  (cl-flet ((predicate (tty)
              (when (string-match-p regex (cdr tty))
                (list (car tty)))))
    (sort
     (seq-mapcat #'predicate (kai/list-serial-ports t))
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
  (cond
   ((eq system-type 'windows-nt) (kai/find-serial-devices "Microsoft +USB Serial Device"))
   (t (kai/find-serial-devices "Microchip Technology Inc"))))

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


(defun kai/aim-tti-powsup-get-devs ()
  "Find all AIM-TTi power supplies."
  (cond
   ((eq system-type 'windows-nt) (kai/find-serial-devices "Microsoft +USB Serial Device"))
   (t (kai/find-serial-devices "TTi QPX1200"))))

(defun kai/aim-tti-powsup-add-process-filter (tty)
  (cl-flet ((filterfunc (proc s)
              (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
                (goto-char (point-max))
                (insert s)
                (if (and (>= (point-max) 1)
                         (string-equal
                          (buffer-substring (- (point-max) 1) (point-max))
                          ""))
                    (insert "\n")))))
    (add-function
     :before
     (process-filter (get-buffer-process tty)) #'filterfunc)))

(defun kai/aim-tti-powsup-instruct (tty instruction)
  "Send INSTRUCTION to AIM-TTi power supply at TTY."
  (unless (and (get-buffer tty)
               (get-buffer-process tty))
    (serial-term tty 9600)
    (kai/aim-tti-powsup-add-process-filter tty)
    (bury-buffer))
  (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
    (goto-char (point-max))
    (insert "INSTR: " instruction "\n"))
  (with-current-buffer tty
    (term-send-string nil (format "%s\n" instruction))))

(defun kai/aim-tti-powsup-on (tty)
  "Power on device at TTY."
  (interactive (list (kai/read-tty-path (kai/aim-tti-powsup-get-devs))))
  (kai/aim-tti-powsup-instruct tty "OP1 1"))

(defun kai/aim-tti-powsup-off (tty)
  "Power off device at TTY."
  (interactive (list (kai/read-tty-path (kai/aim-tti-powsup-get-devs))))
  (kai/aim-tti-powsup-instruct tty "OP1 0"))

(defun kai/aim-tti-powsup-powercycle (tty)
  "Power cycle device at TTY."
  (interactive (list (kai/read-tty-path (kai/aim-tti-powsup-get-devs))))
  (kai/aim-tti-powsup-off tty)
  (run-at-time 2 nil #'kai/aim-tti-powsup-on tty))


(defun kai/aim-tti-powsup-status (tty)
  "Get status of power supply at TTY."
  (interactive (list (kai/read-tty-path (kai/aim-tti-powsup-get-devs))))
  (kai/aim-tti-powsup-instruct tty "V1O?")
  (with-timeout (0.8 "*timeout*")
    (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
      (while (not (and (string-match-p "INSTR: V1O?"
                                       (kai/buffer-get-n-last-line 2))
                       (string-match-p "$"
                                       (kai/buffer-get-n-last-line 1))))
        (sit-for 0.1))
      (kai/aim-tti-powsup-instruct tty "I1O?")
      (while (not (and (string-match-p "INSTR: I1O?"
                                       (kai/buffer-get-n-last-line 2))
                       (string-match-p "$"
                                       (kai/buffer-get-n-last-line 1))))
        (sit-for 0.1))
      (let ((reply (format "ACT: %s %s"
                           (substring (kai/buffer-get-n-last-line 3) 0 6)
                           (substring (kai/buffer-get-n-last-line 1) 0 6))))
        (if (called-interactively-p)
            (message reply)
          reply)))))


(defun kai/manson-powsup-get-devs ()
  "Find all Manson power supplies."
  (kai/find-serial-devices "Silicon Labs CP210"))

(defun kai/manson-powsup-add-process-filter (tty)
  (cl-flet ((filterfunc (proc s)
              (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
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

(defun kai/manson-powsup-instruct (tty instruction)
  "Send INSTRUCTION to Manson power supply at TTY."
  (unless (and (get-buffer tty)
               (get-buffer-process tty))
    (serial-term tty 9600)
    (kai/manson-powsup-add-process-filter tty)
    (bury-buffer))
  (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
    (goto-char (point-max))
    (insert "INSTR: " instruction "\n"))
  (with-current-buffer tty
    (term-send-string nil (format "%s\r" instruction))))

(defun kai/manson-powsup-on (tty)
  "Power on device at TTY."
  (interactive (list (kai/read-tty-path (kai/manson-powsup-get-devs))))
  (kai/manson-powsup-instruct tty "SOUT0"))

(defun kai/manson-powsup-off (tty)
  "Power off device at TTY."
  (interactive (list (kai/read-tty-path (kai/manson-powsup-get-devs))))
  (kai/manson-powsup-instruct tty "SOUT1"))

(defun kai/manson-powsup-powercycle (tty)
  "Power cycle device at TTY."
  (interactive (list (kai/read-tty-path (kai/manson-powsup-get-devs))))
  (kai/manson-powsup-off tty)
  (run-at-time 2 nil #'kai/manson-powsup-on tty))

(defun kai/buffer-get-n-last-line (n)
  "Return the N last line as a string."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- 0 n))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun kai/manson-powsup-status (tty)
  "Get status of power supply at TTY."
  (interactive (list (kai/read-tty-path (kai/manson-powsup-get-devs))))
  (kai/manson-powsup-instruct tty "GETD")
  (with-timeout (0.5 "*timeout*")
    (with-current-buffer (get-buffer-create (format "*powsup replies of %s*" tty))
      (while (not (and (string-match-p "INSTR: GETD"
                                       (kai/buffer-get-n-last-line 2))
                       (string-match-p "OK$"
                                       (kai/buffer-get-n-last-line 1))))
        (sit-for 0.1))
      (let ((reply (apply #'format "ACT: %s.%sV %s.%sA"
                          (seq-subseq
                           (seq-partition (kai/buffer-get-n-last-line 1) 2)
                           0 4))))
        (if (called-interactively-p)
            (message reply)
          reply)))))


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

(defun kai/list-serial-ports (&optional allp)
  (seq-filter
   (lambda (x) (or allp (not (member (car x) kai/serial-ignored-devices))))
   (kai/cached
    #'kai/list-serial-ports-uncached
    5)))

(defun kai/list-serial-ports-uncached ()
  "Returns a LIST of CONS of serial-ports and description."
  (if (eq system-type 'windows-nt)
      (with-temp-buffer
        (call-process "powershell" nil t nil "-NoProfile" "-NonInteractive" "Get-CimInstance Win32_PnPEntity | Where-Object {$_.Caption -match 'COM[0-9]'} | Select-Object Manufacturer, Name")
        (mapcan
         (lambda (l)
           (if (string-match-p "COM[0-9]" l)
               (list (cons
                      (replace-regexp-in-string ".*(\\(COM[0-9]+\\)).*" "\\\\\\\\.\\\\\\1" l)
                      (replace-regexp-in-string
                       "  +"
                       " "
                       (replace-regexp-in-string "\\(.*\\)(\\(COM[0-9]+\\)).*" "\\2 — \\1" l))))))
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
     (sort (directory-files "/dev" t "tty\\(USB\\|ACM\\)[0-9]+") :lessp 'string-version-lessp))))


(defun kai/show-shortcuts ()
  "Open a buffer with buttons for all applicable power-supply functions."
  (interactive)
  (let ((buf (get-buffer-create "*kais shortcuts*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "#+title: Shortcuts\n\n")

      ;; ADP functions
      (when-let ((adp-devs (kai/adp-get-devs)))
        (insert "* Qualcomm ADP\n\n")
        (dolist (tty adp-devs)
          (insert (format "** Device: %s\n\n" (file-name-base tty)))
          (insert (format "[[elisp:(kai/adp-pwr-on \"%s\")][Power On]]  /  " tty))
          (insert (format "[[elisp:(kai/adp-pwr-off \"%s\")][Power Off]]\n\n" tty))
          (insert (format "[[elisp:(kai/adp-reboot \"%s\")][Reboot]]\n\n" tty))
          (insert (format "[[elisp:(kai/adp-enter-edl \"%s\")][Enter EDL]]  /  " tty))
          (insert (format "[[elisp:(kai/adp-exit-edl \"%s\")][Exit EDL]]\n\n" tty))))

      ;; Manson Power Supply functions
      (when-let ((manson-devs (kai/manson-powsup-get-devs)))
        (insert "* Manson Power Supply\n\n")
        (dolist (tty manson-devs)
          (insert (format "** Device: %s\n\n" (file-name-base tty)))
          (insert (format "[[elisp:(kai/manson-powsup-on \"%s\")][Power On]]  /  " tty))
          (insert (format "[[elisp:(kai/manson-powsup-off \"%s\")][Power Off]]\n\n" tty))
          (insert (format "[[elisp:(kai/manson-powsup-powercycle \"%s\")][Power Cycle]]\n\n" tty))
          (insert (format "[[elisp:(kai/manson-powsup-status \"%s\")][Status]]\n\n" tty))))

      ;; AIM-TTi Power Supply functions
      (when-let ((aim-devs (kai/aim-tti-powsup-get-devs)))
        (insert "* AIM-TTi Power Supply\n\n")
        (dolist (tty aim-devs)
          (insert (format "** Device: %s\n\n" (file-name-base tty)))
          (insert (format "[[elisp:(kai/aim-tti-powsup-on \"%s\")][Power On]]  /  " tty))
          (insert (format "[[elisp:(kai/aim-tti-powsup-off \"%s\")][Power Off]]\n\n" tty))
          (insert (format "[[elisp:(kai/aim-tti-powsup-powercycle \"%s\")][Power Cycle]]\n\n" tty))
          (insert (format "[[elisp:(kai/aim-tti-powsup-status \"%s\")][Status]]\n\n" tty))))

      (setq buffer-read-only t)
      (org-mode)
      (setq-local org-link-elisp-skip-confirm-regexp "^(kai/.*")
      ;; Set up key bindings for the shortcut buffer
      (define-key (current-local-map) (kbd "TAB") 'org-next-link)
      (define-key (current-local-map) (kbd "<return>") 'org-open-at-point)
      (define-key (current-local-map) (kbd "<backtab>") 'org-previous-link)
      (display-buffer buf))))
