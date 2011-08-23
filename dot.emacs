; -*- mode: Lisp;-*-
;;; Emacs Load Path
(setq load-path (cons "~/emacs" load-path))

(setq load-path (cons "~/emacs/org-7.01h/lisp" load-path))
(setq load-path (cons "~/emacs/org-7.01h/contrib/lisp" load-path))
(require 'org-install)


;; Fonts
(if (>= emacs-major-version 23)
    (set-default-font "Monospace-10"))

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

;;(require 'uniquify)
;;(setq uniquify-buffer-name-style 'reverse)

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


;; gud-mode (debugging with gdb)
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


;; Jump to matching paren if on a paren
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; (global-set-key [f1]          ')
;; (global-set-key [f2]          ')
;; (global-set-key [f3]          ')
;; (global-set-key [f4]          ')
;; (global-set-key [f5]          '(lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [S-f7]        'compile)
;; (global-set-key [f7]          ')
(global-set-key [C-f7]        'kill-compilation)
;;(global-set-key [f8]          ')
;; (global-set-key [S-right]     ')
;; (global-set-key [S-left]      ')
;; (global-set-key [f9]          ')
;; (global-set-key [f10]         ')
;; (global-set-key [S-f10]       ')
;; (global-set-key [f12]         'dabbrev-expand)
;; (define-key esc-map [f12]     'dabbrev-completion)
                                        ; for my pc @ home
;; (global-set-key [S-SPC] 'dabbrev-expand)
;; (global-set-key [C-f12]       ')
;; (global-set-key "\C-x\C-b"    ')

 ; Make Emacs use "newline-and-indent" when you hit the Enter key so
 ; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m"        'newline-and-indent)

 ; capitalize current word (for example, C constants)
(global-set-key "\M-u"        '(lambda () (interactive) (backward-word 1) (upcase-word 1)))


;; Emacs-lisp-mode Improvements

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (modify-syntax-entry ?- "w")       ; now '-' is not considered a word-delimiter
             ))


;;(setq icicle-define-alias-commands-flag nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "evince %o") ("^html?$" "." "netscape %o"))))
 '(bookmark-save-flag 0)
 '(c-basic-offset 2)
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(case-replace t)
 '(column-number-mode t)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-debug nil t)
 '(compilation-environment nil t)
 '(compilation-first-column 1 t)
 '(compilation-process-setup-function nil t)
 '(compilation-scroll-output t)
 '(compilation-skip-to-next-location t t)
 '(compilation-window-height 30)
 '(current-language-environment "UTF-8")
 '(dabbrev-case-replace nil)
 '(default-input-method "rfc1345")
 '(ecb-compile-window-height 20)
 '(ecb-layout-window-sizes (quote ((#("left-analyse" 0 12 (face nil)) (0.26063829787234044 . 0.09090909090909091) (0.26063829787234044 . 0.12727272727272726) (0.26063829787234044 . 0.12727272727272726) (0.26063829787234044 . 0.6363636363636364)) (#("leftright-analyse" 0 17 (face nil)) (0.21030042918454936 . 0.3888888888888889) (0.21030042918454936 . 0.2916666666666667) (0.21030042918454936 . 0.3055555555555556) (0.3090128755364807 . 0.4861111111111111) (0.3090128755364807 . 0.5)))))
 '(ecb-options-version "2.32")
 '(ecb-other-window-behavior (quote only-edit))
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("/home/jonasbu/svn/DEV-INT2-HistDB/packages/commons/components/C3Po/nsp_servers/csc/logmanager")))
 '(ecb-vc-enable-support t)
 '(emacsw32-eol-check-new-files t)
 '(emacsw32-max-frames t)
 '(emacsw32-mode nil)
 '(emacsw32-style-frame-title t)
 '(gdb-create-source-file-list nil)
 '(global-auto-revert-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(graphviz-dot-auto-indent-on-braces t)
 '(graphviz-dot-view-edit-command nil)
 '(grep-files-aliases (quote (("asm" . "*.[sS]") ("c" . "*.c") ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++") ("ch" . "*.[ch]") ("el" . "*.el") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("cch" . "*.c *.cc *.h *.[ch]xx *.[ch]pp *.[CH] *.CC *.HH *.[ch]++"))))
 '(icicle-Completions-window-max-height 40)
 '(icicle-anything-transform-candidates-flag t)
 '(icicle-download-dir "~/emacs/icicles")
 '(icicle-list-join-string "
")
 '(icicle-reminder-prompt-flag 0)
 '(indent-region-mode t)
 '(menu-bar-mode t)
 '(nxhtml-global-minor-mode t)
 '(nxhtml-global-validation-header-mode t)
 '(nxhtml-load t)
 '(pop-up-windows t)
 '(ps-paper-type (quote a4))
 '(recentf-mode t)
 '(show-paren-mode t nil (paren))
 '(split-width-threshold nil)
 '(tooltip-mode nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(use-file-dialog nil)
 '(make-backup-files nil)
 '(ns-command-modifier (quote meta)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(setq cua-enable-cua-keys nil)

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))


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

(iswitchb-mode 1)

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

(add-to-list 'load-path (expand-file-name "~/emacs/yasnippet-0.6.1c"))
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (expand-file-name "~/emacs/yasnippet-0.6.1c/snippets"))

;; (global-set-key [f3] 'grep-find)
;; (setq grep-find-command
;;       "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -I -n -e ")

;; (load-file (expand-file-name "~/emacs/graphviz-dot-mode.el")) 
;; (load-file (expand-file-name "~/emacs/fold-dwim.el")) 

;; (global-set-key (kbd "<f7>")      'fold-dwim-toggle)
;; (global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
;; (global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)


;; Lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)
(add-hook 'lua-mode-hook 'hs-minor-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain) 

;; Ecb and semantic
;;(setq semantic-load-turn-everything-on t)
;;(require 'semantic-load)


;; (setq load-path (cons "~/emacs/xref/emacs" load-path))
;; (setq exec-path (cons "~/emacs/xref" exec-path))
;; (load "xrefactory")

;; SQLite
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

(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(require 'ido)
(ido-mode t)

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

(autoload 'company-mode "company" nil t)

(put 'downcase-region 'disabled nil)


(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
(add-hook 'c-mode-common-hook 'google-make-newline-indent)



 ;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)                     ; for all buffers
(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only


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

(setq emerge-diff-options "--ignore-all-space")


;;--------------------------------------------------------------------
;; Lines enabling gnuplot-mode

;; move the files gnuplot.el to someplace in your lisp load-path or
;; use a line like
(setq load-path (append (list "/home/jonasbu/emacs/gnuplot-mode.0.6.0") load-path))

;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)

(add-to-list 'Info-default-directory-list "/home/jonasbu/emacs/gnuplot-mode.0.6.0")


;; end of line for gnuplot-mode
;;--------------------------------------------------------------------

;;(require 'ess-site)


;;; Emacs Load Path
(winner-mode 1)

;; (load-file "/usr/share/emacs/site-lisp/xcscope.el")
;; (require 'xcscope)
;; (require 'xcscope+)

;; from http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/

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


;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(require 'whitespace)
(setq whitespace-style '(face indentation trailing empty lines-tail))
(setq whitespace-line-column nil)
(set-face-attribute 'whitespace-line nil
                    :background "purple"
                    :foreground "white"
                    :weight 'bold)
;; (global-whitespace-mode 1)



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

(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

(setq mac-option-modifier 'none)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(transient-mark-mode t)
(setq query-replace-highlight t)
(setq default-major-mode 'text-mode) 

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
