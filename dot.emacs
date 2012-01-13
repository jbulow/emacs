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
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

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


;; (global-set-key (quote [C-tab]) (quote next-buffer))
;; (global-set-key (quote [C-S-tab]) (quote previous-buffer))
(global-set-key (quote [S-tab]) (quote other-window))

;;; auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
;; (ac-config-default)
;; (ac-set-trigger-key "TAB")

;;; yasnippet
(require 'yasnippet) 
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory (concat emacs-root "yasnippet/snippets"))

;;; Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;;; Python stuff
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (load-library "init_python")

;; ----------------------------------------------------------- [ ibuffer ]
;; *Nice* buffer switching
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("version control" (or (mode . svn-status-mode)
                    (mode . svn-log-edit-mode)
                    (name . "^\\*svn-")
                    (name . "^\\*vc\\*$")
                    (name . "^\\*Annotate")
                    (name . "^\\*git-")
                    (name . "^\\*vc-")))
         ("emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^TAGS\\(<[0-9]+>\\)?$")
                      (name . "^\\*Help\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*Occur\\*$")
                      (name . "^\\*grep\\*$")
                      (name . "^\\*Compile-Log\\*$")
                      (name . "^\\*Backtrace\\*$")
                      (name . "^\\*Process List\\*$")
                      (name . "^\\*gud\\*$")
                      (name . "^\\*Man")
                      (name . "^\\*WoMan")
                      (name . "^\\*Kill Ring\\*$")
                      (name . "^\\*Completions\\*$")
                      (name . "^\\*tramp")
                      (name . "^\\*shell\\*$")
                      (name . "^\\*compilation\\*$")))
         ("emacs source" (or (mode . emacs-lisp-mode)
                             (filename . "/Applications/Emacs.app")
                             (filename . "/bin/emacs")))
         ("agenda" (or (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (name . "^\\*Agenda")
                       (name . "^\\*org-")
                       (name . "^\\*Org")
                       (mode . org-mode)
                       (mode . muse-mode)))
         ("latex" (or (mode . latex-mode)
                      (mode . LaTeX-mode)
                      (mode . bibtex-mode)
                      (mode . reftex-mode)))
         ("dired" (or (mode . dired-mode))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Order the groups so the order is : [Default], [agenda], [emacs]
(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                 activate)
  (setq ad-return-value (nreverse ad-return-value)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pager)
(global-set-key "\C-v"     'pager-page-down)
(global-set-key [next]     'pager-page-down)
(global-set-key "\ev"      'pager-page-up)
(global-set-key [prior]    'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(global-set-key '[S-f7]  'compile)


;; Haskell

(load "~/emacs/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(server-start)

