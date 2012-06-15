; -*- mode: Lisp; tab-width: 2; -*-
;;; Emacs Load Path

(defvar home-dir (cond ((eq system-type 'darwin) "~/")
                       ((eq system-type 'cygwin) "~/")
                       ((eq system-type 'gnu/linux) "~/")
                       ((eq system-type 'windows-nt) "~/")
                       ;;                       ((eq system-type 'windows-nt) (concat "c:/Users/" user-login-name))
                       "My home directory"))

(when (eq system-type 'windows-nt)
  (setq default-directory home-dir))


(defvar dropbox-dir (cond ((eq system-type 'darwin) "~/Dropbox/")
                          ((eq system-type 'cygwin) "~/")
                          ((eq system-type 'gnu/linux) "~/Dropbox/")
                          ((eq system-type 'windows-nt) "~/../../Dropbox/")
;;                       ((eq system-type 'windows-nt) (concat "c:/Users/" user-login-name))
		       "My Dropbox directory"))



(require 'cl) ;; Common Lisp 

(defvar emacs-root (concat home-dir "emacs/")
  "My emacs home directory - the root of my personal emacs load-path.")

;;
;; Additional load paths
;;
(labels ((add-path (p)
                   (add-to-list 'load-path
                                (concat emacs-root p))))
  (add-path "")
  (add-path "org-7.8.01/lisp")
  (add-path "org-7.8.01/contrib/lisp")
  (add-path "yasnippet-0.6.1c")
  (add-path "auctex-11.86")
  (add-path "ipa")
  (add-path "anything-config"))


;;
;; Additional exec paths (for windows). Prepend on path to make real 'find'
;; command override dos 'find' command.
;;
;; TODO: cond on system-type windows-nt
(defun prepend-exec-path (path)
  ""
  (let ((path_w_semicolon (concat path ";")))
    (setenv "PATH" (concat path_w_semicolon (getenv "PATH")))
    (setq exec-path (cons path exec-path ))))

;; GoW: https://github.com/bmatzelle/gow/wiki
(if (file-directory-p "C:\\Program Files (x86)\\Gow\\bin")
    (prepend-exec-path "C:\\Program Files (x86)\\Gow\\bin"))

;; Git for Windows: http://code.google.com/p/msysgit/
(if (file-directory-p "C:\\Program Files (x86)\\Git\\bin")
    (prepend-exec-path "C:\\Program Files (x86)\\Git\\bin"))

;;
;; org-mode
;;
(require 'org-install)
(require 'org-latex)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)                     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")



;;
;; Emacs Custom set Variables and Faces
;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-view-predicate-list (quote ((darwin-system (eq system-type (quote darwin))) (linux-system (eq system-type (quote gnu/linux))) (windows-system (eq system-type (quote windows-nt))))))
 '(TeX-view-program-list (quote (("Open" "open %o") ("AcroRead" "start %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") ((output-pdf linux-system) "Evince") ((output-pdf darwin-system) "Open") ((windows-system output-pdf) "AcroRead"))))
 '(anything-for-files-prefered-list (quote (anything-c-source-ffap-line anything-c-source-ffap-guesser anything-c-source-buffers+ anything-c-source-recentf anything-c-source-bookmarks anything-c-source-file-cache anything-c-source-files-in-current-dir+ anything-c-source-locate)))
 '(column-number-mode t)
 '(grep-files-aliases (quote (("asm" . "*.[sS]") ("c" . "*.c") ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++") ("ch" . "*.[ch]") ("el" . "*.el") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("cch" . "*.c *.cc *.h *.[ch]xx *.[ch]pp *.[CH] *.CC *.HH *.[ch]++"))))
 '(large-file-warning-threshold 30000000)
 '(make-backup-files nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((compilation-search-path (list (nil "/disk/jonasbu/DEV-References-clean/packages/commons/components/C3Po/"))) (compilation-search-path (list (nil "/disk/jonasbu/DEV-References/packages/commons/components/C3Po/"))) (compilation-search-path (list (nil "/disk/jonasbu/DEV-References-INT/packages/commons/components/C3Po/"))))))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(split-width-threshold nil)
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;;
;; Other variables
;;
(setq mac-option-modifier 'none)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(transient-mark-mode t)
(setq query-replace-highlight t)
(setq default-major-mode 'text-mode) 
(setq cua-enable-cua-keys nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq emerge-diff-options "--ignore-all-space")

; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; will make "Ctrl-k" kills an entire line if the cursor is at the beginning of line 
(setq kill-whole-line t) 

;; will delete "hungrily" in C mode
(setq c-hungry-delete-key t)

;; Fonts
;;(if (>= emacs-major-version 23)
;;    (set-default-font "Monospace-10"))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; Source:  http://www-db.stanford.edu/~manku/dotemacs.html

;;(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)

;; (setq next-line-add-newlines nil) will disallow creation of new lines when you press the "arrow-down key" at end of the buffer. 

;;  load auto-show (shows lines when cursor moves to right of long line).
;;(require 'auto-show) (auto-show-mode 1) (setq-default auto-show-mode t)

;; will introduce spaces instead of tabs by default.
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)

;; will position the cursor to end of output in shell mode.
;;(auto-show-make-point-visible) 

;; will highlight during query.
(setq query-replace-highlight t)

;;highlight incremental search
(setq search-highlight t) 

;; will make text-mode default.
(setq default-major-mode 'text-mode) 

;; means that we want fontification in all modes.
;;(global-font-lock-mode t t) 

;; denotes our interest in maximum possible fontification.
(setq font-lock-maximum-decoration t)

;; allow recursive editing in minibuffer
(setq enable-recursive-minibuffers t) 

;; follow-mode allows easier editing of long files 
;;(follow-mode t)                       





;; Make unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

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

;;(setq c-auto-newline t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GYP support.  Requires python.el (as opposed to python-mode.el).
;;
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

;;; Python stuff
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (load-library "init_python")

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
;;(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory (concat emacs-root "yasnippet-0.6.1c/snippets"))

(require 'dropdown-list)
(setq yas/prompt-functions
      '(yas/dropdown-prompt
        yas/ido-prompt
        yas/x-prompt
        yas/completing-prompt
        yas/no-prompt))
;;
;; Ido mode
;;
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))

(global-set-key [f12] 'ido-find-file-in-tag-files)


(defvar ido-enable-replace-completing-read nil
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

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; Ido complete on M-x
(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))




;;
;; Hippie-expand
;;
(require 'hippie-exp)

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

;;(global-set-key [f12]         'dabbrev-expand)
;;(define-key esc-map [f12]     'dabbrev-completion)



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



;; Haskell

(load "~/emacs/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;
;; gnuplot
;;

;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like
(setq load-path (append (list "~/emacs/gnuplot-mode.0.6.0") load-path))

;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)

(add-to-list 'Info-default-directory-list "~/emacs/gnuplot-mode.0.6.0")




;; XXXXX


;;
;; cc-mode
;;
(require 'cc-mode)

;;(setq compilation-window-height 8)

;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (string-match "exited abnormally" str)
;;             ;;there were errors
;;             (message "compilation errors, press C-x ` to visit")
;;           ;;no errors, make the compilation window go away in 0.5 seconds
;;           (run-at-time 0.5 nil 'delete-windows-on buf)
;;           (message "NO COMPILATION ERRORS!"))))

;; Customizing indentation
;;
;; First of all, put (require 'cc-mode) atop the C customization section
;; of your ~/.emacs, so the methods below will be defined. Then, while
;; visiting a C source file, if you don't like how a particular line is
;; indenting, press C-c C-o near the expression you want to change, and
;; figure out the symbol to change (pressing C-c C-o gives you a chance
;; to set these variables interactively). Then, do a
;; (c-set-offset 'symbol-to-change X) in your ~/.emacs where X is
;; typically a numeric value or the symbol '+ or '-. The following is an
;; example of some of my customizations, which is simply my personal
;; preference:
(c-set-offset 'substatement-open 0)
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'topmost-intro-cont '+)
(c-set-offset 'inline-open 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'substatement-open 0)

;; ;; Here's a sample .emacs file fragment that might help you along the
;; ;; way.  Just copy this region and paste it into your .emacs file.
;; ;; You might want to change some of the actual values.

;; ;; Make some non-standard key bindings.  We can put these in
;; ;; c-mode-base-map because c-mode-map, c++-mode-map, and so on,
;; ;; inherit from it.
;; (defun my-c-initialization-hook ()
;;   (define-key c-mode-base-map "\C-m" 'c-context-line-break)
;;   (define-key c-mode-base-map [?\C-\M-a] 'c-beginning-of-defun)
;;   (define-key c-mode-base-map [?\C-\M-e] 'c-end-of-defun)
;; (add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; ;; offset customizations not in my-c-style
;; ;; This will take precedence over any setting of the syntactic symbol
;; ;; made by a style.
;; (setq c-offsets-alist '((member-init-intro . ++)))

;; ;; Create my personal style.
;; (defconst my-c-style
;;   '((c-tab-always-indent        . t)
;;     (c-comment-only-line-offset . 4)
;;     (c-hanging-braces-alist     . ((substatement-open after)
;;                                    (brace-list-open)))
;;     (c-hanging-colons-alist     . ((member-init-intro before)
;;                                    (inher-intro)
;;                                    (case-label after)
;;                                    (label after)
;;                                    (access-label after)))
;;     (c-cleanup-list             . (scope-operator
;;                                    empty-defun-braces
;;                                    defun-close-semi))
;;     (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
;;                                    (substatement-open . 0)
;;                                    (case-label        . 4)
;;                                    (block-open        . 0)
;;                                    (knr-argdecl-intro . -)))
;;     (c-echo-syntactic-information-p . t))
;;   "My C Programming Style")
;; (c-add-style "PERSONAL" my-c-style)

;; ;; Customizations for all modes in CC Mode.
;; (defun my-c-mode-common-hook ()
;;   ;; set my personal style for the current buffer
;;   (c-set-style "PERSONAL")
;;   ;; other customizations
;;   (setq tab-width 8
;;         ;; this will make sure spaces are used instead of tabs
;;         indent-tabs-mode nil)
;;   ;; we like auto-newline, but not hungry-delete
;;   (c-toggle-auto-newline 1))
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (autoload 'ibuffer "ibuffer" "List buffers." t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; google C Style
;; todo: replace generic cc-mode above
;;
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;
;; tempo
;;



;;
;; Fast Find other File
;;

(setq ff-other-file-alist
      '(("\\.cc\\'"  (".hh" ".h"))
        ("\\.hh\\'"  (".cpp" ".cc" ".C"))

        ("\\.c\\'"   (".h"))
        ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.m\\'"    (".h"))
        ("\\.mm\\'"    (".h"))

        ("\\.C\\'"   (".H"  ".hh" ".h"))
        ("\\.H\\'"   (".C"  ".CC"))

        ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH\\'"  (".CC"))

        ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
        ("\\.h\\+\\+\\'" (".c++"))

        ("\\.cpp\\'" (".hpp" ".hh" ".h"))
        ("\\.hpp\\'" (".cpp"))

        ("\\.cxx\\'" (".hxx" ".hh" ".h"))
        ("\\.hxx\\'" (".cxx"))))


(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-o" 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'my-c-initialization-hook)

(add-to-list 'load-path (expand-file-name "~/emacs/find-things-fast"))
(require 'find-things-fast)
;; (global-set-key '[f1] 'ftf-find-file)
;; (global-set-key '[f2] 'ftf-grepsource)
;; (global-set-key '[f4] 'ftf-gdb)
;; (global-set-key '[f5] 'ftf-compile)

(setq ff-search-directories 
      '("."
        "../../source/*"
        "../interface/*"
        "../include/*"
        "../../interface/*"
        "../../include/*"))

(global-set-key '[S-f7]  'compile)
(global-set-key '[C-f7] 'kill-compilation)

 ; Make Emacs use "newline-and-indent" when you hit the Enter key so
 ; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m"        'newline-and-indent)

 ; capitalize current word (for example, C constants)
(global-set-key "\M-u"        '(lambda () (interactive) (backward-word 1) (upcase-word 1)))


;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (turn-on-auto-fill)
;;              (setq fill-column 80)
;;              (setq comment-column 60)
;;              (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
;;              (c-set-style "ellemtel")           ; set indentation style
;;              (local-set-key [(control tab)]     ; move to next tempo mark
;;                             'tempo-forward-mark)
;;              ))

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))


;;
;; Emacs-lisp-mode Improvements
;;

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (modify-syntax-entry ?- "w")       ; now '-' is not considered a word-delimiter
             ))



;;
;;
;;

;; (iswitchb-mode 1) ;; Disables/breaks vertical listing in ido-mode when enabled

;; (add-to-list 'load-path (expand-file-name "~/emacs/icicles"))
(add-to-list 'load-path (expand-file-name "~/emacs/icicles"))
;(require 'icicles)
;(require 'icomplete+)
;;(require 'icicles-iswitchb)
;(icicle-mode 1)    ; Turn on Icicle mode.

(put 'upcase-region 'disabled nil)

;;(load (concat nxml-path "rng-auto.el"))

(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss" "xaml") t) "\\'")
                   'nxml-mode))

(unify-8859-on-decoding-mode)

(setq magic-mode-alist
      (cons '("<＼＼?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(fset 'html-mode 'nxml-mode)

(put 'scroll-left 'disabled nil)

;; (global-set-key [f3] 'grep-find)
;; (setq grep-find-command
;;       "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -I -n -e ")

;; (load-file (expand-file-name "~/emacs/graphviz-dot-mode.el")) 
;; (load-file (expand-file-name "~/emacs/fold-dwim.el")) 

;; (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
;; (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
;; (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)


;; 
;; Lua
;;
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain) 

;;
;; SQLite
;;
(setq sql-sqlite-program "/usr/bin/sqlite3")
(setq sql-user nil)
(setq sql-password nil)

 ;; autoinsert C/C++ header
(require 'autoinsert)
;;(auto-insert-mode)
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "//\n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
              "#define " ident  " 1\n\n\n"
              "\n\n#endif // " ident "\n"))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "// \n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat nopath ".h")))
      (if (file-exists-p ident)
          (concat "#include \"" ident "\"\n")))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

;; (require 'jump)
;; (global-set-key [f4] 'jump-symbol-at-point)
;; (global-set-key [(shift f4)] 'jump-back)

;; (if (eq window-system 'x)
;;     (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))
;; (global-set-key [f13] 'execute-extended-command)


;;
;; company-mode
;;

(autoload 'company-mode "company" nil t)

(put 'downcase-region 'disabled nil)

(defadvice kill-buffer (around my-kill-buffer-check activate)
  "Prompt when a buffer is about to be killed."
  (let* ((buffer-file-name (buffer-file-name))
         backup-file)
    ;; see 'backup-buffer
    (if (and (buffer-modified-p)
             buffer-file-name
             (file-exists-p buffer-file-name)
             (setq backup-file (car (find-backup-file-name buffer-file-name))))
        (let ((answer (completing-read (format "Buffer modified %s, (d)iff, (s)ave, (k)ill? " (buffer-name))
                                       '("d" "s" "k") nil t)))
          (cond ((equal answer "d")
                 (set-buffer-modified-p nil)
                 (let ((orig-buffer (current-buffer))
                       (file-to-diff (if (file-newer-than-file-p buffer-file-name backup-file)
                                         buffer-file-name
                                       backup-file)))
                   (set-buffer (get-buffer-create (format "%s last-revision" (file-name-nondirectory file-to-diff))))
                   (buffer-disable-undo)
                   (insert-file-contents file-to-diff nil nil nil t)
                   (set-buffer-modified-p nil)
                   (setq buffer-read-only t)
                   (ediff-buffers (current-buffer) orig-buffer)))
                ((equal answer "k")
                 (set-buffer-modified-p nil)
                 ad-do-it)
                (t
                 (save-buffer)
                 ad-do-it)))
      ad-do-it)))



;;  When activated, it allows to “undo” (and “redo”) changes in the
;;  window configuration with the key commands ‘C-c left’ and ‘C-c
;;  right’
(winner-mode 1)

;;
;; recent file mode
;;

(require 'recentf)
 
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
 
;; enable recent files mode.
(recentf-mode t)
 
; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;
;; Whitespace
;;
(require 'whitespace)
(setq whitespace-style '(face indentation trailing empty lines-tail))
(setq whitespace-line-column nil)
(set-face-attribute 'whitespace-line nil
                    :background "purple"
                    :foreground "white"
                    :weight 'bold)
;; (global-whitespace-mode 1)





;; Improve zap-to-char (M-z) by not deleting the boundary character
;; I.e zap-to-char '>' on <Xabcdef> (cursor on X) gives <>.

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))


;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (c-set-style "stroustrup")                ; set indentation style
;;              (turn-on-auto-fill)
;;              (setq c-basic-offset 2)
;;              (setq fill-column 80)
;;              (setq comment-column 60)
;; ;;             (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
;;              ))

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))

;;  The function below is funky but useful. Having swapped the pairs
;;  ('[', '{'), ('-', '_') and (']', '}'), in order to type "->", we
;;  need to type four characters ('Shift' followed by '-' followed by
;;  'Shift' followed by '>'). With the above code, all you need to
;;  type is two underscores: '__'). Automagically, they are converted
;;  into '->'). Similarly, two successive dots '..' are translated
;;  into '[]' (for array indexing). I find that these combinations
;;  improve my code-typing speed significantly.


(defun my-editing-function (first last len)
  (interactive)
  (if (and (boundp 'major-mode)
           (member major-mode (list 'c-mode 'c++-mode 'gud-mode 'fundamental-mode 'ruby-mode))
           (= len 0)
           (> (point) 4)
           (= first (- (point) 1)))      
      (cond
       ((and (string-equal (buffer-substring (point) (- (point) 2)) "__")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
        (progn (delete-backward-char 2) (insert-char ?- 1) (insert-char ?> 1)))

       ((string-equal (buffer-substring (point) (- (point) 3)) "->_")
        (progn (delete-backward-char 3) (insert-char ?_ 3)))
       
       ((and (string-equal (buffer-substring (point) (- (point) 2)) "..")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "...")))
        (progn (delete-backward-char 2) (insert-char ?[ 1) (insert-char ?] 1) (backward-char 1))) 

       ((and (string-equal (buffer-substring (point) (- (point) 2)) ",,")
             (not (string-equal (buffer-substring (point) (- (point) 3)) ",,,")))
        (progn (delete-backward-char 2) (insert-char ?{ 1) (insert-char ?} 1) (backward-char 1))) 

       ((and (> (point-max) (point))
             (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "[.]"))
        (progn (forward-char 1) (delete-backward-char 3) (insert-char ?. 1) (insert-char ?. 1) ))
       )      
    nil))

(add-hook 'after-change-functions 'my-editing-function)

;;
;; gud-mode (debugging with gdb)
;;
(setq gdb-many-windows nil)

(add-hook 'gud-mode-hook
	  '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt
                            'comint-bol)
	     (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))


;; 
;; Jump to matching paren if on a paren
;;
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)





;;; YYYY 

;;;
;;; OS X stuff
;;; 
;; (setq ps-lpr-command "~/bin/ps_print_file.sh")

;; (add-to-list 'load-path "/Applications/MacPorts/Emacs.app/Contents/Resources/lisp/" t)


;;
;; auctex
;;
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

(setq TeX-save-query nil) ;;autosave before compiling

(add-hook 'LaTeX-mode-hook '(lambda ()
                              (TeX-fold-mode 1)
                              (outline-minor-mode 1)
                              ))

(add-hook 'LaTeX-mode-hook '(lambda () (setq fill-column 72)))
;;(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-DVI-via-PDFTeX t)))

 	
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)


;; (require 'tex-site)
;; (server-start)
;; (add-hook 'server-switch-hook
;;           (lambda nil
;;             (let ((sevrver-buf (current-buffer)))
;;               (bury-buffer)
;;               (switch-to-buffer-other-frame server-buf))))

;; (add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

;; (setq TeX-output-view-style
;;       (quote
;;         (
;;          ("^dvi$" "." "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient %o")
;;          ("^pdf$" "." "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient %o")
;;          ("^html?$" "." "open -A Safari.app %o")
;;          )
;;         )
;;        )


;;
;; go mode
;;

(require 'go-mode-load)


;;
;; deft
;;

(when (require 'deft nil 'noerror) 
   (setq
      deft-extension "org"
      deft-directory (concat dropbox-dir "deft/")
      deft-text-mode 'org-mode)
   (global-set-key (kbd "<f9>") 'deft))


;;
;; etags related
;;

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)



;;
;; END
;;

(server-start)



;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

;;
;; Package repositories
;;
(when (require 'package nil 'noerror)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/")))

;;
;; anything
;;


;; (require 'anything-config)
;; 
;; (global-set-key (kbd "C-c I")  ;; i -> info
;;   (lambda () (interactive)
;;     (anything
;;       :prompt "Info about: "
;;       :candidate-number-limit 3
;;       :sources
;;       '( anything-c-source-info-libc             ;; glibc docs
;;          anything-c-source-man-pages             ;; man pages
;;          anything-c-source-info-emacs))))        ;; emacs
;; 
;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda()
;;   ;; other stuff...
;;   ;; ...
;;   ;; put useful info under C-c i
;;     (local-set-key (kbd "C-c i")
;;       (lambda() (interactive)
;;         (anything
;;           :prompt "Info about: "
;;           :candidate-number-limit 5
;;           :sources
;;           '( anything-c-source-emacs-functions
;;              anything-c-source-emacs-variables
;;              anything-c-source-info-elisp
;;              anything-c-source-emacs-commands
;;              anything-c-source-emacs-source-defun
;;              anything-c-source-emacs-lisp-expectations
;;              anything-c-source-emacs-lisp-toplevels
;;              anything-c-source-emacs-functions-with-abbrevs
;;              anything-c-source-info-emacs))))))
;;    
