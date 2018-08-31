(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
                                        ;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq user-full-name "Marc Ziegler"
      user-email-adress "marc.ziegler@uk-erlangen.de")

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq gc-cons-threshold (* 1024 1024 100))
(setq max-lisp-eval-depth 5000)

(setq max-specpdl-size 5000)
(setq debug-on-error nil)

;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; for fill column mode
(setq-default fill-column 100)
(setq mode-require-final-newline t)
(setq sentence-end-double-space nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq global-mark-ring-max 5000)
(setq mark-ring-max 5000)
(delete-selection-mode)

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])

(setq custom-file "~/.emacs.d/emacs-custom.el")
(defalias 'yes-or-no-p 'y-or-n-p)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(display-time-mode t)
(column-number-mode t)
(global-prettify-symbols-mode t)
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
            )
      )

(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(load-theme 'CrapCram t)
(set-face-attribute 'default nil :height 95)

(if (eq system-type 'windows-nt)
    (set-face-font 'default "-outline-Consolas-normal-normal-normal-mono-13-*-*-*-c-*-fontset-auto4")
  (set-face-font 'default "-1ASC-Liberation Mono-normal-italic-normal-*-*-*-*-*-m-0-iso10646-1"))

(use-package rainbow-delimiters
:ensure t
:config
    (add-hook 'fundamental-mode-hook 'rainbow-delimiters-mode)
    (custom-set-faces
     '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "dark orange"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "chartreuse"))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground "dark green"))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground "cyan"))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground "blue"))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground "magenta"))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground "sienna")))))
)

(use-package hlinum
:ensure t
:config
    (hlinum-activate)
)

(use-package smart-mode-line
:ensure t
:config
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup)
)

(use-package indent-guide
:ensure t
:config
    (indent-guide-global-mode 1)
)

(defun mz/emacs-reload()
  "Reload the emacs ini file (~/.emacs.d/init.el)"
  (interactive)
  (load-file '"~/.emacs.d/init.el"))

(defun mz/indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (point-to-register 'o)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (jump-to-register 'o))

(defun mz/new-line-above ()
  "Insert an empty line above the current line.
                  Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun mz/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(defun mz/print-list (list)
  (dotimes (item (length list))
    (insert (prin1-to-string (elt list item)))
    (insert " ")))

(defun mz/write-package-install ()
  (insert "
                  (unless package-archive-contents
                    (package-refresh-contents))
                  (setq pp '())
                  (dolist (p package-archive-contents)
                          (push (car p) pp))
                  (dolist (package mypackages)
                    (unless (package-installed-p package)
                      (if (member package pp) (package-install package))))"
          ))

(defun mz/print-package-list ()
  (interactive)
  (find-file package-file)
  (erase-buffer)
  (insert "(defvar mypackages '(")
  (mz/print-list package-activated-list)
  (insert "))")
  (mz/write-package-install)
  (save-buffer)
  (kill-buffer))

(defun mz/my_compile ()
  "Take the makefile in current folder or in build folder"
  (interactive)
  (if (file-exists-p "Makefile")
      (progn
        (setq compile-command "make -j4")
        )
    (progn
      (setq compile-command
            (concat "cd " (replace-regexp-in-string "src" "build" (file-name-directory buffer-file-name)) " && make -j4"))))
  (compile compile-command))

(defun mz/workwndw()
  "Load specific files and the window accordingly"
  (interactive)
  (find-file "~/Stuff/ToDo/todo.org")
  (split-window-right)
  (find-file "~/Stuff/ToDo/agenda.org")
  (split-window-below)
  (find-file "~/Stuff/ToDo/worktime.org")
  (windmove-right)
  (outline-show-all))

(defun mz/fast-calc()
  "Parse for ++$1++ and substiute with the calculated result of $1."
  (interactive)
  (save-excursion)
  (beginning-of-buffer)
  (while (re-search-forward "\\+\\+" nil t)
    (progn
      (beginning-of-buffer)
      (when (re-search-forward "\\+\\+[ \\.0-9\\+\\(\\)\\*\\/\\-]+\\+\\+" nil t)
        (setf
         (point) (match-beginning 0)
         (mark) (match-end 0)))
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (replace-string "++" "")
        (exchange-point-and-mark)
        (replace-string
         (buffer-substring (region-beginning) (region-end))
         (calc-eval (buffer-substring (region-beginning) (region-end))))))))


(defun mz/next-buffer()
  "Go to the next buffer and continue if the buffername starts with * (except scratch)"
  (interactive)
  (let ((currbuffer-name (buffer-name)))
    (next-buffer)
    (buffer-name)
    (while
        (and (string-match "^\\*.*\\*$" (buffer-name))
             (not (string-match "^\\*scratch\\*$" (buffer-name)))
             (not (string-match currbuffer-name (buffer-name))))
      (next-buffer))))

(defun mz/previous-buffer()
  "Go to the previous buffer and continue if the buffername starts with * (except scratch)"
  (interactive)
  (let ((currbuffer-name (buffer-name)))
    (previous-buffer)
    (buffer-name)
    (while
        (and (string-match "^\\*.*\\*$" (buffer-name))
             (not (string-match "^\\*scratch\\*$" (buffer-name)))
             (not (string-match currbuffer-name (buffer-name))))
      (previous-buffer))))

(defun mz/mark-everything-in-parenthesis()
  (interactive)
  (sp-beginning-of-sexp)
  (set-mark-command nil)
  (sp-end-of-sexp))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package smartparens
  :ensure t
  :config
  (setq sp-base-key-bindings 'paredit)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1)
  )

(use-package multiple-cursors
  :ensure t)

(use-package clean-aindent-mode
  :ensure t
  :config
  (set 'clean-aindent-is-simple-indent t)
  )

;; Package: ws-butler
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package dictcc
  :ensure t)

(use-package winner
  :ensure t
  :init
  (winner-mode)
  :bind ((  "M-g <prior>" . winner-undo)
     ("M-g <next>" . winner-redo)
     :map winner-mode-map
     ("C-c <left>" . nil)
     ("C-c <right>" . nil))
  )

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

(use-package epc
  :ensure t)

(use-package flyspell
  :ensure t
  :config
  (use-package auto-dictionary
    :ensure t
    :init
    (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))
  (use-package writegood-mode
    :ensure t
    :init
    (add-hook 'flyspell-mode-hook (lambda () (writegood-mode 1)))))

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-elisp)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode 1)
  (setq company-idle-delay 'nil)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :bind (:map yas-keymap
              ("<return>" . yas/exit-all-snippets)
              ("C-e" . (lambda()
                         (interactive)
                         (let* ((snippet (car (yas--snippets-at-point)))
                                (position (yas--field-end (yas--snippet-active-field snippet))))
                           (if (= (point) position)
                               (move-end-of-line 1)
                             (goto-char position)))))
              ("C-a" . (lambda()
                         (interactive)
                         (let* ((snippet (car (yas--snippets-at-point)))
                                (position (yas--field-start (yas--snippet-active-field snippet))))
                           (if (= (point) position)
                               (move-beginning-of-line 1)
                             (goto-char position))))))
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

(use-package magit
:ensure t)

(use-package flycheck
:ensure t
:config
(global-flycheck-mode 1))

(use-package rtags
  :ensure t
  :config
  (progn
    ;; Start rtags upon entering a C/C++ file
    (add-hook
     'c-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
               (rtags-start-process-unless-running))))
    (add-hook
     'c++-mode-common-hook
     (lambda () (if (not (is-current-file-tramp))
               (rtags-start-process-unless-running))))
    ;; Flycheck setup
    (use-package flycheck-rtags
      :ensure t
      :config
      (defun my-flycheck-rtags-setup ()
        (flycheck-select-checker 'rtags)
        ;; RTags creates more accurate overlays.
        (setq-local flycheck-highlighting-mode nil)
        (setq-local flycheck-check-syntax-automatically nil))
      )
    (use-package helm-rtags
      :ensure t
      :config
      (setq rtags-display-result-backend 'helm)
      )
    ;; c-mode-common-hook is also called by c++-mode
    (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
    )
  )
;; Use irony for completion
(use-package irony
  :ensure t
  :config
  (progn
    (add-hook
     'c-mode-common-hook
     (lambda () (if (not (is-current-file-tramp)) (irony-mode))))
    (add-hook
     'c++-mode-common-hook
     (lambda () (if (not (is-current-file-tramp)) (irony-mode))))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (use-package company-irony
      :ensure t
      :config
      (push 'company-irony company-backends)
      )
    (use-package company-irony-c-headers
      :ensure t
      :config
      (add-to-list 'company-backends 'company-c-headers)
      (add-to-list 'company-backends 'company-irony-c-headers)
      (add-to-list 'company-backends 'company-clang)
      )
    ))

;; setup GDB
(setq gdb-many-windows t ;; use gdb-many-windows by default
      gdb-show-main t  ;; Non-nil means display source file containing the main routine at startup
      )

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  (setq c-default-style "linux" )
  (setq c-basic-offset 4)
  (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
  (setq irony-cdb-compilation-databases '(irony-cdb-libclang irony-cdb-clang-complete))
  (rtags-start-process-unless-running)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (hs-minor-mode)
  (rainbow-mode)
  (rainbow-delimiters-mode)
  (turn-on-auto-fill)
  (global-set-key [f6] 'run-cfile)
  (global-set-key [C-c C-y] 'uncomment-region)
  )

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'R-mode-hook #'rainbow-delimiters-mode)
(add-hook 'R-mode-hook #'rainbow-mode)
(add-hook 'R-mode-hook 'hs-minor-mode)

(use-package julia-mode
:ensure t
:init
(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode))
:config
  (add-hook 'julia-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'julia-mode-hook 'hs-minor-mode)
       (use-package flycheck-julia
  :ensure t)
  (use-package julia-shell
  :ensure t))

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-to-list 'auto-mode-alist '("\\.el$" . lisp-interaction-mode))
(add-hook 'lisp-interaction-mode 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode 'hs-minor-mode)

(use-package slime
:ensure t
:config
(setq inferior-lisp-program "/usr/bin/sbcl")
)

(use-package gnuplot-mode
:ensure t
:config
(use-package gnuplot
:ensure t
:config
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

  (add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
  (add-to-list 'auto-mode-alist '("\\.plt$" . gnuplot-mode))

  (add-hook 'gnuplot-mode-hook
            (lambda () (local-set-key (kbd "C-c C-c") 'gnuplot-run-buffer)))
  (add-hook 'gnuplot-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'gnuplot-mode-hook #'rainbow-mode)
  (add-hook 'gnuplot-mode-hook 'hs-minor-mode)
  ))

(add-hook 'shell-script-mode-hook #'rainbow-delimiters-mode)
(add-hook 'shell-script-mode-hook #'rainbow-mode)
(add-hook 'sh-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sh-mode-hook #'rainbow-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist '(sh-mode "\\(do\\|then\\|in\\)" "\\(done\\|fi\\|esac\\|elif\\)" "/[*/]" nil nil))

(use-package python
    :mode ("\\.py\\'" . python-mode)
    ("\\.wsgi$" . python-mode)
    :interpreter ("python" . python-mode)
    :init
    (setq-default indent-tabs-mode nil)
    :config
    (setq python-indent-offset 4)

    (use-package py-autopep8
:ensure t)

    (add-hook 'python-mode-hook 'smartparens-mode)
    (add-hook 'python-mode-hook 'rainbow-mode)
    (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'python-mode-hook 'ede-mode)
    (add-hook 'python-mode-hook 'turn-on-auto-fill)
    (add-hook 'python-mode-hook 'hs-minor-mode)
    )


  (use-package jedi
    :ensure t
    :config
    (use-package company-jedi
:ensure t
:init
(add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
(setq company-jedi-python-bin "python")))


  (use-package anaconda-mode
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    :config (use-package company-anaconda
  :ensure t
  :init (add-hook 'python-mode-hook 'anaconda-mode)
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

  (use-package elpy
    :ensure t
    :commands elpy-enable
    :init (with-eval-after-load 'python (elpy-enable))

    :config
    (electric-indent-local-mode -1)
    (delete 'elpy-module-highlight-indentation elpy-modules)
    (delete 'elpy-module-flymake elpy-modules)

    (defun ha/elpy-goto-definition ()
(interactive)
(condition-case err
    (elpy-goto-definition)
  ('error (xref-find-definitions (symbol-name (symbol-at-point))))))

    :bind (:map elpy-mode-map ([remap elpy-goto-definition] .
       ha/elpy-goto-definition)))

(use-package auctex
     :ensure t
     :mode (("\\.tex\\'" . latex-mode)
      ("\\.sty\\'" . latex-mode))
     :commands (latex-mode LaTeX-mode plain-tex-mode)
     :init
     (progn
 (defun my-latex-mode-hook()
   (TeX-fold-mode 1)
   (hs-minor-mode)
   (add-hook 'find-file-hook 'TeX-fold-buffer t t)
   (local-set-key [C-c C-g] 'TeX-kill-job)
   (turn-on-auto-fill)
   (rainbow-delimiters-mode)
   (rainbow-mode)
   (TeX-source-correlate-mode)
   (turn-on-reftex)
   (LaTeX-math-mode)
   (LaTeX-preview-setup)
   (flyspell-mode 1)
   )

 (setq TeX-auto-save t
       TeX-parse-self t
       TeX-save-query nil
       TeX-PDF-mode t
       TeX-master nil
       TeX-engine 'xetex
       latex-run-command "xelatex --shell-escape"
       reftex-plug-into-AUCTeX t)
 )
     :config
     (use-package company-auctex
 :ensure t
 :config
 (company-auctex-init)
 )
     (use-package company-bibtex
 :ensure t
 :config
 (add-to-list 'company-backends 'company-bibtex))

     (TeX-add-style-hook
"latex"
(lambda ()
  (LaTeX-add-environments
   '("frame" LaTeX-env-contents))))

     (setq TeX-view-program-selection
     (quote
      (((output-dvi style-pstricks)
  "dvips and gv")
       (output-dvi "xdvi")
       (output-pdf "Okular")
       (output-html "xdg-open"))))
     (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) --shell-escape %S%(PDFout)")))))

(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

(use-package sgml-mode
:ensure t)
     (add-to-list 'hs-special-modes-alist
                  '(nxml-mode
                    "<!--\\|<[^/>]*[^/]>"
                    "-->\\|</[^/>]*[^/]>"

                    "<!--"
                    sgml-skip-tag-forward
                    nil))
     (add-hook 'nxml-mode-hook 'hs-minor-mode)

(if (eq system-type 'windows-nt)
    (setq org-directory "C:/zieglemc/Stuff/ToDo")
  (setq org-directory "/home/zieglemc/Stuff/ToDo"))

(define-obsolete-function-alias 'org-define-error 'define-error)
(defun org-file-path (filename)
  "Return the absolute adress of an org file, given its relative name"
  (interactive)
  (message "%s" (concat (file-name-as-directory org-directory) filename))
  )

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s" ))

(setq org-reveal-root "file:///home/zieglemc/src/reveal.js-master/js/reveal.js")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(setq org-hide-leading-stars t)
(setq org-ellipsis " ↷")
(use-package org-bullets
:ensure t
)

(defun my-org-mode-hook ()
  (org-bullets-mode 1)
  (hs-minor-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  )

(add-hook 'org-mode-hook 'my-org-mode-hook)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-agenda-custom-commands
      '(("W" "Show entries for 3 weeks" agenda "" ((org-agenda-span 21)))))

(setq org-agenda-files `(
                         ,(org-file-path "worktime.org")
                         ,(org-file-path "todo.org")
                         ,(org-file-path "ideas.org")
                         ,(org-file-path "to-read.org")
                         ,(org-file-path "agenda.org")
                         ))

(setq org-log-done 'time)
(define-key global-map "\C-c\C-x\C-s" 'mz/mark-done-and-archive)

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "firefox %s")
        ("\\.pdf\\'" . "okular \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "okular \"%s\"")
        ("\\.nrrd\\'" . "vv %s")
        ("\\.jpg\\'" . "gpicview %s")
        ("\\.raw\\'" . "imagej %s")
        ("\\.png\\'" . "gpicview $s")))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t) (ruby . t) (gnuplot . t) (python . t) (gnuplot . t) (shell . t) (org . t) (lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)

(setq org-export-coding-system 'utf-8)

(use-package ox-reveal
  :ensure t)
(use-package ox-twbs
  :ensure t)
(use-package ox-pandoc
  :ensure t
  :config
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  )
(use-package org-ref
  :ensure t)

(setq reftex-default-bibliography '("~/Documents/Literature/bibliography.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Documents/Literature/Papers.org"
org-ref-default-bibliography '("~/Documents/Literature/bibliography.bib")
org-ref-pdf-directory "~/Documents/Literature/bibtex-pdfs/")

(setq bibtex-completion-bibliography "~/Documents/Literature/bibliography.bib"
bibtex-completion-library-path "~/Documents/Literature/bibtex-pdfs/"
bibtex-completion-notes-path "~/Documents/Literature/helm-bibtex-notes")

(use-package helm-bibtex
  :ensure t
  :config
  (setq helm-bibtex-format-citation-functions
  '((org-mode . (lambda (x) (insert (concat
       "[[bibentry:"
       (mapconcat 'identity x ",")
       "]]")) "")))))

(add-to-list 'org-modules 'org-drill)
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-hint-separator "|")
(setq org-drill-left-cloze-delimiter "<[")
(setq org-drill-right-cloze-delimiter "]>")
(setq org-drill-learn-fraction 0.25)
(load-file "~/.emacs.d/mz-functions/learnjapanese.el")

(setq mz/todo-file (org-file-path "todo.org"))
(setq mz/ideas-file (org-file-path "ideas.org"))
(setq mz/to-read-file (org-file-path "to-read.org"))
(setq mz/how-to-file (org-file-path "how-to.org"))
(setq mz/agenda-file (org-file-path "agenda.org"))

(setq org-capture-templates
      '(
        ("t" "Todo"
         entry
         (file mz/todo-file))
        ("i" "Ideas"
         entry
         (file mz/ideas-file))
        ("r" "To Read"
         checkitem
         (file mz/to-read-file))
        ("h" "How-To"
         entry
         (file mz/how-to-file))
        ))

(setq jp/vocabulary-file (org-file-path "Vocabulary.org"))
(add-to-list 'org-capture-templates
             '("j" "Japanese Word/Phrase" entry (file+headline jp/vocabulary-file "Words and Phrases")
               "** %(jp/type-prompt)     :drill:\n   :PROPERTIES:\n   :DRILL_CARD_TYPE: multisided\n   :ADDED:    %U\n   :END:\n*** Japanese\n    %(jp/japanese-get-word (jp/japanese-prompt))\n*** English\n    %(jp/english-prompt)"))
(add-to-list 'org-capture-templates
             '("J" "Japanese Grammar" entry (file+headline jp/vocabulary-file "Grammar")
               "** %(jp/grammar-type-prompt) :drill:\n   :PROPERTIES:\n   :DRILL_CARD_TYPE: hide2cloze\n   :ADDED:    %U\n   :END:\n   %(jp/definition-prompt)\n*** Example\n    %(jp/japanese-get-word (jp/japanese-prompt))\n    %(jp/english-prompt)"))
(add-to-list 'org-capture-templates
             '("a" "Agenda Entry" entry (file mz/agenda-file)
               "* %^{Appointment}            %^G\n  %^T\n%?"))

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "<f12>") 'eval-buffer)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f5>") 'mz/my_compile)
(global-set-key (kbd "M-+") 'mz/fast-calc)
(global-set-key (kbd "M-o") 'mz/new-line-above)
(global-set-key "\C-x\\" 'mz/indent-buffer)
(global-unset-key (kbd "C-x <left>"))
(global-set-key (kbd "C-x <left>") 'mz/previous-buffer)
(global-unset-key (kbd "C-x <right>"))
(global-set-key (kbd "C-x <right>") 'mz/next-buffer)

(fset 'make_newline
      [?\C-e tab return])
(global-set-key (kbd "C-<return>") 'make_newline)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
(global-set-key (kbd "C-<tab>") 'helm-company)
(define-key global-map (kbd "C-.") 'company-files)
(global-set-key (kbd "C-!") 'repeat)
(global-set-key (kbd "C-x g") 'magit-status)


(global-set-key (kbd "<C-m> d") 'dictcc)
(global-set-key (kbd "<C-m> D") 'dictcc-at-point)
;; movement between different frames
(global-set-key (kbd "M-g <left>") 'windmove-left)
(global-set-key (kbd "M-g <right>") 'windmove-right)
(global-set-key (kbd "M-g <up>") 'windmove-up)
(global-set-key (kbd "M-g <down>") 'windmove-down)

;; smartparens bindings
(global-set-key (kbd "M-p a") 'sp-beginning-of-sexp)
(global-set-key (kbd "M-p e") 'sp-end-of-sexp)
(global-set-key (kbd "M-p <down>") 'sp-down-sexp)
(global-set-key (kbd "M-p <up>") 'sp-up-sexp)
(global-set-key (kbd "M-p f") 'sp-forward-sexp)
(global-set-key (kbd "M-p b") 'sp-backward-sexp)
(global-set-key (kbd "M-p n") 'sp-next-sexp)
(global-set-key (kbd "M-p r") 'sp-rewrap-sexp)
(global-set-key (kbd "M-p <left>") 'sp-backward-slurp-sexp)
(global-set-key (kbd "M-p <right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-p C-<left>") 'sp-backward-barf-sexp)
(global-set-key (kbd "M-p C-<right>") 'sp-previous-barf-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") nil)
(define-key smartparens-mode-map (kbd "C-<right>") nil)
(define-key smartparens-mode-map (kbd "M-r") nil)
(define-key smartparens-mode-map (kbd "M-s") nil)
(global-set-key (kbd "M-p t") 'sp-transpose-sexp)
(global-set-key (kbd "M-p k") 'sp-kill-sexp)
(global-set-key (kbd "M-p ( ")  'wrap-with-parens)
(global-set-key (kbd "M-p [ ")  'wrap-with-brackets)
(global-set-key (kbd "M-p { ")  'wrap-with-braces)
(global-set-key (kbd "M-p ' ")  'wrap-with-single-quotes)
(global-set-key (kbd "M-p _ ")  'wrap-with-underscores)
(global-set-key (kbd "M-p ` ")  'wrap-with-back-quotes)
(global-set-key (kbd "M-p d") 'sp-unwrap-sexp)
(global-set-key (kbd "M-p m") 'mz/mark-everything-in-parenthesis)

;; multiple cursors
(global-set-key (kbd "M-n <right>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-n <left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n C-<right>") 'mc/skip-to-next-like-this)
(global-set-key (kbd "M-n C-<left>") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-n <") 'mc/unmark-next-like-this)
(global-set-key (kbd "M-n >") 'mc/unmark-previous-like-this)
(global-set-key (kbd "M-n a") 'mc/mark-all-like-this)

;; sr-speedbar
(global-set-key (kbd "M-g f") 'sr-speedbar-toggle)

;; ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; hide and show region
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h a") 'hs-hide-all)
(global-set-key (kbd "M-h <tab>") 'hs-toggle-hiding)
(global-set-key (kbd "M-h s a") 'hs-show-all)
(global-set-key (kbd "M-h r") 'hs-hide-block)
(global-set-key (kbd "M-h s r") 'hs-show-block)

;; rtags
(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r d") 'rtags-find-symbol-at-point)
(global-set-key (kbd "M-r f") 'rtags-find-symbol)
(global-set-key (kbd "M-r <left>") 'rtags-location-stack-back)
(global-set-key (kbd "M-r <right>") 'rtags-location-stack-forward)
(global-set-key (kbd "M-r l") 'rtags-taglist)
(global-set-key (kbd "M-r r") 'rtags-rename-symbol)
(global-set-key (kbd "M-r p") 'rtags-reparse-file)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-s") 'helm-swoop)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key org-mode-map (kbd "C-<tab>") nil)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key org-mode-map (kbd "C-c <left>") 'org-metaleft)
(define-key org-mode-map (kbd "C-c <right>") 'org-metaright)
(define-key org-mode-map (kbd "C-c <up>") 'org-metaup)
(define-key org-mode-map (kbd "C-c <down>") 'org-metadown)
(define-key org-mode-map (kbd "C-c S-<left>") 'org-metashiftleft)
(define-key org-mode-map (kbd "C-c S-<right>") 'org-metashiftright)
(define-key org-mode-map (kbd "C-c S-<up>") 'org-metashiftup)
(define-key org-mode-map (kbd "C-c S-<down>") 'org-metashiftdown)
(define-key org-mode-map (kbd "C-c <left>") 'org-metaleft)
(define-key org-mode-map (kbd "C-c <right>") 'org-metaright)
(define-key org-mode-map (kbd "C-c <up>") 'org-metaup)
(define-key org-mode-map (kbd "C-c <down>") 'org-metadown)
(define-key org-mode-map (kbd "C-c S-<left>") 'org-metashiftleft)
(define-key org-mode-map (kbd "C-c S-<right>") 'org-metashiftright)
(define-key org-mode-map (kbd "C-c S-<up>") 'org-metashiftup)
(define-key org-mode-map (kbd "C-c S-<down>") 'org-metashiftdown)

(define-key org-mode-map (kbd "C-c C-r") nil)
(define-key org-mode-map (kbd "C-c C-r b") 'org-ref-helm-insert-cite-link)
(define-key org-mode-map (kbd "C-c C-r r") 'org-ref-helm-insert-ref-link)

(global-set-key (kbd "<f10>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-step);; equiv matlab step in
(global-set-key (kbd "<f8>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

;; this is down here because it destroyes parens matching and coloring
(global-set-key (kbd "M-p \" ") 'wrap-with-double-quotes)

(if (file-exists-p "~/PATIENTS/PatDB.el")
    (load-file "~/PATIENTS/PatDB.el")
  )
