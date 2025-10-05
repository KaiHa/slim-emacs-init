;;; -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'font-lock-function-name-face))
 '(calendar-today-visible-hook '(calendar-mark-today))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(custom-enabled-themes '(leuven))
 '(history-length 1000)
 '(icomplete-vertical-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(org-babel-load-languages '((emacs-lisp . t) (R . t) (shell . t)))
 '(package-archive-priorities '(("gnu" . 9) ("nongnu" . 8) ("melpa-stable" . 7)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages nil)
 '(savehist-mode t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editable zone starts below ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bash-completion
  :ensure t
  :config
  (bash-completion-setup)
  :hook
  (eshell-mode . (lambda ()
                   (add-hook 'completion-at-point-functions
                             'bash-completion-capf-nonexclusive nil t))))
(use-package cmake-mode
  :ensure t)

(use-package ibuffer :ensure nil
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Directories" (mode . dired-mode))
           ("Emacs" (or (mode . emacs-lisp-mode)
                        (mode . diary-mode)
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Help\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*scratch\\*$")))
           ("Org" (or (mode . org-mode)
                      (mode . org-agenda-mode)))
           ("Internal" (name . "^\\*.*\\*$"))
           ("Magit" (or (mode . magit-blame-mode)
                        (mode . magit-cherry-mode)
                        (mode . magit-diff-mode)
                        (mode . magit-log-mode)
                        (mode . magit-process-mode)
                        (mode . magit-status-mode)))
           ("Other" (predicate identity t)))
          ("file-status"
           ("Modified" (and (visiting-file) (modified)))
           ("Unmodified" (and (visiting-file) (not modified)))
           ("Not visiting a file" (not visiting-file))))
        ibuffer-display-summary nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-default-sorting-mode 'alphabetic
        ibuffer-title-face 'font-lock-doc-face)
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "default")))
  :bind
  (("C-x C-b" . ibuffer)))

(use-package iedit
  :ensure t
  :bind
  (("C-;" . iedit-mode)))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . #'magit-status))
  :custom
  (magit-diff-refine-hunk t))

(use-package ob-tmux
  :ensure t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")
     (:session . "default")
     (:socket  . nil)))
  (org-babel-tmux-session-prefix "ob-"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kai/serial-term (tty)
  (if-let* ((buf (get-buffer tty))
            (_proc (get-buffer-process buf)))
      (pop-to-buffer buf)
    (serial-term tty 115200)))

(defun kai/adp-instruct (instruction)
  (let ((tty "/dev/ttyACM0"))
    (unless (and (get-buffer tty)
                 (get-buffer-process tty))
      (serial-term tty 115200)
      (bury-buffer))
    (with-current-buffer tty
      (term-send-string nil instruction)
      (term-send-input))))

(defun kai/adp-enter-edl ()
  (interactive)
  (kai/adp-instruct "PWR_OFF 1")
  (run-at-time 1 nil #'kai/adp-instruct "MD_EDL 1")
  (run-at-time 2 nil #'kai/adp-instruct "PWR_OFF 0"))

(defun kai/adp-exit-edl ()
  (interactive)
  (kai/adp-instruct "PWR_OFF 1")
  (run-at-time 1 nil #'kai/adp-instruct "MD_EDL 0")
  (run-at-time 2 nil #'kai/adp-instruct "PWR_OFF 0"))

(defun kai/adp-pwr-on ()
  (interactive)
  (kai/adp-instruct "PWR_OFF 0"))

(defun kai/adp-pwr-off ()
  (interactive)
  (kai/adp-instruct "PWR_OFF 1"))

(defun kai/adp-reboot ()
  (interactive)
  (kai/adp-instruct "PWR_OFF 1")
  (run-at-time 2 nil #'kai/adp-instruct "PWR_OFF 0"))

(defun kai/powsup-get-dev ()
  (cl-flet ((powsupp (tty)
              (string-search "ID_VENDOR=Silicon_Labs"
                             (shell-command-to-string
                              (format "udevadm info --name=%s" tty)))))
    (seq-some (lambda (n)
                (let ((tty (format "/dev/ttyUSB%d" n)))
                  (if (powsupp tty)
                      tty
                    nil)))
              (number-sequence 0 10))))

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

(defun kai/powsup-instruct (instruction)
  (let ((tty (kai/powsup-get-dev)))
    (unless (and (get-buffer tty)
                 (get-buffer-process tty))
      (serial-term tty 9600)
      (kai/powsup-add-process-filter tty)
      (bury-buffer))
    (with-current-buffer (get-buffer-create "*powsup replies*")
      (goto-char (point-max))
      (insert "INSTR: " instruction "\n"))
    (with-current-buffer tty
      (term-send-string nil (format "%s\r" instruction)))))

(defun kai/powsup-on ()
  (interactive)
  (kai/powsup-instruct "SOUT0"))

(defun kai/powsup-off ()
  (interactive)
  (kai/powsup-instruct "SOUT1"))

(defun kai/powsup-powercycle ()
  (interactive)
  (kai/powsup-off)
  (run-at-time 2 nil #'kai/powsup-on))

(defun kai/buffer-get-n-last-line (n)
  "Return the N last line as a string."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- 0 n))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun kai/powsup-status ()
  (interactive)
  (kai/powsup-instruct "GETD")
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
    (mapcan
     (lambda (n)
       (let ((tty  (format "/dev/ttyUSB%d" n)))
         (if (file-exists-p tty)
             (list (cons
                    tty
                    (format "%s — %s"
                            (file-name-base tty)
                            (replace-regexp-in-string
                             "_"
                             " "
                             (replace-regexp-in-string
                              "\\(.*\n\\)*.*ID_SERIAL=\\(.*\\)\\(.*\n\\)*"
                              "\\2"
                              (shell-command-to-string
                               (format "udevadm info --name=%s" tty))))))))))
     (number-sequence 0 10))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup powsup nil "Remote control of Manson power supplies.")

(defcustom powsup-show-status
  nil
  "Don't show the status of the powersupply in the menu if nil, otherwise show it.
Showing the status blocks the serial port of the power supply as soon as Emacs runs."
  :type 'boolean
  :group 'powsup)

(easy-menu-define kais-toolbox-menu nil "Kais Toolbox Menu"
  '("Kais-Toolbox"
    ("Power Supply"
     :active (kai/powsup-get-dev)
     :label (if (kai/powsup-get-dev)
                (format "Power Supply (%s)" (file-name-base (kai/powsup-get-dev)))
              "Power Supply")
     ["Status" nil
      :visible powsup-show-status
      :label (kai/powsup-status)
      :active nil]
     ["Power On" kai/powsup-on t]
     ["Power Off" kai/powsup-off t]
     ["Power-Cycle (off/on)" kai/powsup-powercycle t]
     ["Show Status Message" (message "power supply - %s" (kai/powsup-status)) t])
    ("ADP"
     :active (file-exists-p "/dev/ttyACM0")
     ["Power On" kai/adp-pwr-on t]
     ["Power Off" kai/adp-pwr-off t]
     ["Reboot" kai/adp-reboot t]
     "--"
     ["Emergency Download Mode (EDL)" nil nil]
     ["Enter EDL" kai/adp-enter-edl t]
     ["Exit EDL" kai/adp-exit-edl t])))

;;; Fill the "Serial Terminal" menu
(defun kai/get-serial-menu ()
  (easy-menu-create-menu
   "Serial Term"
   (append
    '(["Send File..." kai/send-file-over-serial-to-qnx
       :active (kai/serial-term-buffer-p)]
      "--")
    (mapcar
     (lambda (tty)
       `[,(car tty)
         (kai/serial-term ,(car tty))
         :label ,(cdr tty)])
     (kai/list-serial-ports)))))

(defun kai/update-menu ()
  (easy-menu-add-item kais-toolbox-menu nil (kai/get-serial-menu)))

(add-hook 'menu-bar-update-hook #'kai/update-menu)

(keymap-set-after
  (lookup-key global-map [menu-bar])
  "<Kais-Toolbox>"
  (cons "Kais-Toolbox" kais-toolbox-menu) t)
