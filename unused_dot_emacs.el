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
