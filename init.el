;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (file-name-concat
                         (file-name-directory (or load-file-name buffer-file-name))
                         "lisp"))
(require 'libkai)

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
 '(tmm-completion-prompt nil)
 '(tmm-shortcut-words nil)
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

(use-package company
  :ensure t
  :custom
  (global-company-mode t)
  (company-idle-delay 0.5))

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
;;; Menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup powsup nil "Remote control of power supplies.")

(defcustom powsup-show-status
  nil
  "Don't show the status of the powersupply in the menu if nil, otherwise show it.
Showing the status blocks the serial port of the power supply as soon as Emacs runs."
  :type 'boolean
  :group 'powsup)

(defvar *kai/tty-info* (make-hash-table) "Session settings for the serial devices.")

(easy-menu-define kais-toolbox-menu nil "Kais Toolbox Menu"
  '("Kais-Toolbox"))

(defun kai/get-powsup-menu ()
  "Generate menu items for all power-supplies."
  (append
   (mapcar
    (lambda (tty)
      (easy-menu-create-menu
       (format "Manson Power Supply — %s" (file-name-base tty))
       `(["Status" nil
          :visible powsup-show-status
          :label (kai/manson-powsup-status ,tty)
          :active nil]
         ["Power On" (kai/manson-powsup-on ,tty) t]
         ["Power Off" (kai/manson-powsup-off ,tty) t]
         ["Power-Cycle (off/on)" (kai/manson-powsup-powercycle ,tty) t]
         ["Show Status Message" (message "power supply - %s" (kai/manson-powsup-status ,tty)) t])))
    (kai/manson-powsup-get-devs))
   (mapcar
    (lambda (tty)
      (easy-menu-create-menu
       (format "AIM-TTi Power Supply — %s" (file-name-base tty))
       `(["No, this NOT a power supply" (puthash ,tty 'none *kai/tty-info*)
          :style radio :selected (not (kai/aim-tti-p ,tty)) :visible (kai/windowsp)]
         ["Yes, this is a power supply" (puthash ,tty 'aim-tti *kai/tty-info*)
          :style radio :selected (kai/aim-tti-p ,tty) :visible (kai/windowsp)]
         "--"
         ["Status" nil
          :visible powsup-show-status
          :label (kai/aim-tti-powsup-status ,tty)
          :active nil]
         ["Power On" (kai/aim-tti-powsup-on ,tty) :active (kai/aim-tti-p ,tty)]
         ["Power Off" (kai/aim-tti-powsup-off ,tty) :active (kai/aim-tti-p ,tty)]
         ["Power-Cycle (off/on)" (kai/aim-tti-powsup-powercycle ,tty) :active (kai/aim-tti-p ,tty)]
         ["Show Status Message" (message "power supply - %s" (kai/aim-tti-powsup-status ,tty)) :active (kai/aim-tti-p ,tty)])))
    (kai/aim-tti-powsup-get-devs))))

(defun kai/adpp (tty)
  (if (kai/windowsp)
      (equal 'adp (gethash tty *kai/tty-info*))
    t))

(defun kai/aim-tti-p (tty)
  (if (kai/windowsp)
      (equal 'aim-tti (gethash tty *kai/tty-info*))
    t))

(defun kai/windowsp ()
  (eq system-type 'windows-nt))

(defun kai/get-adp-menu ()
  "Generate menu items for all ADP devices."
  (mapcar
   (lambda (tty)
     (easy-menu-create-menu
      (format "Qualcomm ADP — %s" (file-name-base tty))
      `(["No, this NOT an ADP" (puthash ,tty 'none *kai/tty-info*)
         :style radio :selected (not (kai/adpp ,tty)) :visible (kai/windowsp)]
        ["Yes, this is an ADP" (puthash ,tty 'adp *kai/tty-info*)
         :style radio :selected (kai/adpp ,tty) :visible (kai/windowsp)]
        "--"
        ["Power On" (kai/adp-pwr-on ,tty) :active (kai/adpp ,tty)]
        ["Power Off" (kai/adp-pwr-off ,tty) :active (kai/adpp ,tty)]
        ["Reboot" (kai/adp-reboot ,tty) :active (kai/adpp ,tty)]
        "--"
        ["Emergency Download Mode (EDL)" nil nil]
        ["Enter EDL" (kai/adp-enter-edl ,tty) (kai/adpp ,tty)]
        ["Exit EDL" (kai/adp-exit-edl ,tty) (kai/adpp ,tty)])))
   (kai/adp-get-devs)))

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
     (kai/list-serial-ports))
    '(["Open all of the above serial terms" kai/serial-open-all
       :active (kai/list-serial-ports)]))))

(defun kai/update-menu ()
  (mapcar
   (lambda (x) (easy-menu-remove-item kais-toolbox-menu nil (nth 2 x)))
   (seq-drop (easy-menu-get-map kais-toolbox-menu nil) 2))
  (easy-menu-add-item kais-toolbox-menu nil (kai/get-serial-menu))
  (mapcar (lambda (x) (easy-menu-add-item kais-toolbox-menu nil x)) (kai/get-adp-menu))
  (mapcar (lambda (x) (easy-menu-add-item kais-toolbox-menu nil x)) (kai/get-powsup-menu)))

(add-hook 'menu-bar-update-hook #'kai/update-menu)

(keymap-set-after
  (lookup-key global-map [menu-bar])
  "<Kais-Toolbox>"
  (cons "Kais-Toolbox" kais-toolbox-menu) t)
