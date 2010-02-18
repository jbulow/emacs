(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ns-command-modifier (quote meta))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq mac-option-modifier 'none)
(setq inhibit-startup-message t)

(setq require-final-newline t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(transient-mark-mode t)

(setq query-replace-highlight t)

(setq default-major-mode 'text-mode) 

;; allow recursive editing in minibuffer
;; (setq enable-recursive-minibuffers t) 

(require 'uniquify)
(setq uniquify-buffer-name-style                          'reverse)

; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; will make "Ctrl-k" kills an entire line if the cursor is at the beginning of line -- very useful.
(setq kill-whole-line t) 

;; will delete "hungrily" in C mode! Use it to see what it does -- very useful.
(setq c-hungry-delete-key t)

;;  will let emacs put in a "carriage-return" for you automatically
;;  after left curly braces, right curly braces, and semi-colons in "C
;;  mode" -- very useful.

(setq c-auto-newline 1)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (turn-on-auto-fill)
             (setq fill-column 80)
             (setq comment-column 60)
             (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
             (c-set-style "ellemtel")           ; set indentation style
             (local-set-key [(control tab)]     ; move to next tempo mark
                            'tempo-forward-mark)
             ))

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))


(setq ps-lpr-command "/Users/jonas/bin/ps_print_file.sh")

(add-to-list 'load-path "/Users/jonas/emacs" t)
(require 'go-mode-load)


;;(require 'tex-site)

(add-to-list 'load-path "/Applications/MacPorts/Emacs.app/Contents/Resources/lisp/" t)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(add-hook 'LaTeX-mode-hook '(lambda ()
                 (TeX-fold-mode 1)
                 (outline-minor-mode 1)
                   ))

(add-hook 'LaTeX-mode-hook '(lambda () (setq fill-column 72)))
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-DVI-via-PDFTeX t)))

(server-start)
;; (add-hook 'server-switch-hook
;;  	  (lambda nil
;;  	    (let ((sevrver-buf (current-buffer)))
;; 	      (bury-buffer)
;;  	      (switch-to-buffer-other-frame server-buf))))

;; (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

 (setq TeX-output-view-style
	  (quote
	   (
	    ("^dvi$" "." "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient %o")
	    ("^pdf$" "." "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient %o")
	    ("^html?$" "." "open -A Safari.app %o")
	    )
	   )
)


(require 'ido)
(ido-mode t)

 (defvar ido-enable-replace-completing-read t
   "If t, use ido-completing-read instead of completing-read if possible.
    
    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:
    
    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                                     allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))



;; Factor
(load-file "/Applications/factor//misc/fuel/fu.el")
