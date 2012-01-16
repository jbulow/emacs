; -*- mode: Lisp;-*-
;;; Emacs Load Path
(defvar home-dir (cond ((eq system-type 'darwin) "~/")
                       ((eq system-type 'cygwin) "~/")
                       ((eq system-type 'gnu/linux) "~/")
                       ((eq system-type 'windows-nt) "~/")
;;                       ((eq system-type 'windows-nt) (concat "c:/Users/" user-login-name))
		       "My home directory"))
(setq default-directory home-dir)

(require 'cl) ;; Common Lisp 

(defvar emacs-root (concat home-dir "emacs/")
  "My home directory - the root of my personal emacs load-path.")

(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "")
  (add-path "org-7.8.01/lisp")
  (add-path "org-7.8.01/contrib/lisp")
  (add-path "yasnippet"))


(defun prepend-exec-path (path)
  ""
  (let ((path_w_colon (concat path ":")))
    (setenv "PATH" (concat path_w_colon (getenv "PATH")))
    (setq exec-path (cons path_w_colon exec-path ))))


(if (file-directory-p "C:/Program Files (x86)/Gow/bin")
    (prepend-exec-path "C:/Program Files (x86)/Gow/bin"))




; -*- mode: Lisp; tab-width: 2; -*-
(require 'org-install)
(require 'org-latex)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(make-backup-files nil)
 '(ns-command-modifier (quote meta))
 '(show-paren-mode t))

(setq mac-option-modifier 'none)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(transient-mark-mode t)
(setq query-replace-highlight t)
(setq default-major-mode 'text-mode) 

; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; will make "Ctrl-k" kills an entire line if the cursor is at the beginning of line 
(setq kill-whole-line t) 

;; will delete "hungrily" in C mode
(setq c-hungry-delete-key t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GYP support.  Requires python.el (as opposed to python-mode.el).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
(when (string-match "python-mode.el" (symbol-file 'python-mode 'defun))
  (error (concat "python-mode must be loaded from python.el (bundled with "
                 "recent emacsen), not from the older and less maintained "
                 "python-mode.el")))
(defadvice python-calculate-indentation (after ami-outdent-closing-parens
                                               activate)
  "De-indent closing parens, braces, and brackets in python and derived modes."
  (if (string-match "^ *[])}][],)}]* *$"
                    (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position)))
      (setq ad-return-value (- ad-return-value 2))))
(define-derived-mode gyp-mode python-mode "Gyp"
  "Major mode for editing .gyp files."
  (setq python-continuation-offset 2
        python-indent 2
        python-guess-indent nil))
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . gyp-mode))
(add-to-list 'auto-mode-alist '("\\.gypi\\'" . gyp-mode))

;; Default to python3.1
(add-hook 'python-mode-hook
            (lambda ()
              (setq py-python-command "python3.1")
              (setq py-default-interpreter "python3.1")))
