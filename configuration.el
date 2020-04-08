(add-to-list 'package-archives '("stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0))
      ;; Pin a couple of packages to MELPA
      package-pinned-packages
      '(;; I maintain these and know what changes
        ("flycheck"  . "MELPA")))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq user-full-name "Marc Ziegler")

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

(use-package rainbow-mode
  :ensure t)

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
  "Reload the Emacs ini file (~/.emacs.d/init.el)."
  (interactive)
  (load-file '"~/.emacs.d/init.el"))

(defun mz/indent-buffer ()
  "Indent an entire buffer using the default intenting scheme."
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
  "Mark the state of an 'org-mode' item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun mz/print-list (list)
  "A function to print a LIST in a formatted matter."
  (dotimes (item (length list))
    (insert (prin1-to-string (elt list item)))
    (insert " ")))

(defun mz/write-package-install ()
  "Write a function to a file which iterates over a package list and installes missing packages."
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
  "Print the list of all packages installed. This function should not be needed if use-package is used."
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
  "Take the makefile in current folder or in build folder."
  (interactive)
  (if (file-exists-p "Makefile")
      (progn
        (setq compile-command "make -j4")
        )
    (progn
      (setq compile-command
            (concat "cd "
                    (replace-regexp-in-string "src" "build" (file-name-directory buffer-file-name))
                    " && make -j4"))))
  (compile compile-command))

(defun mz/workwndw()
  "Load specific files and the window accordingly."
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
         (calc-eval
          (buffer-substring
           (region-beginning) (region-end))))))))


(defun mz/buffer-skippable (buffername)
  "Check if the BUFFERNAME startes either with '*' or is within the buffer-exceptions."
  (setq star-buffer-exceptions '("^\\*scratch\\*$" "^\\*R\\*$" "^\\*julia\\*$" "^\\*shell\\*$") )
  (setq normal-buffer-exceptions '("^magit-.*$"))
  (setq in-star-buffers nil)
  (setq in-buffer-exceptions nil)

  (dolist (current-restring star-buffer-exceptions in-star-buffers)
    (setq in-star-buffers (cons (not (string-match current-restring buffername)) in-star-buffers)))
  (setq in-star-buffers (cons (string-match "^\\*.*\\*$" buffername) in-star-buffers))

  (dolist (current-restring normal-buffer-exceptions in-buffer-exceptions)
    (setq in-buffer-exceptions (cons (string-match current-restring buffername) in-buffer-exceptions)))

  (or (null (memq nil in-star-buffers)) (null (memq nil in-buffer-exceptions)))
  )

(defun mz/next-buffer()
  "Go to the next buffer and continue if the buffer is skippable according to mz/buffer-skippable."
  (interactive)
  (next-buffer)
  (while (mz/buffer-skippable (buffer-name))
    (next-buffer)))

(defun mz/previous-buffer()
  "Go to the previous buffer and continue if the buffer is skippable according to mz/buffer-skippable."
  (interactive)
  (previous-buffer)
  (while (mz/buffer-skippable (buffer-name))
    (previous-buffer)))

(defun mz/mark-everything-in-parenthesis()
  "Mark everything within parenthesis."
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
  :init (smartparens-global-mode 1)
  :bind (("M-p a" . sp-beginning-of-sexp)
         ("M-p e" . sp-end-of-sexp)
         ("M-p k" . sp-kill-sexp)
         ("M-p d" . sp-unwrap-sexp)
         ("M-p m" . mz/mark-everything-in-parenthesis)
         ("M-p r" . sp-rewrap-sexp)
         :map smartparens-mode-map
         ("C-<left>" . nil)
         ("C-<right>" . nil)
         ("M-r" . nil)
         ("M-s" . nil))
  :config
  (turn-on-smartparens-mode)
  (sp-pair "(" ")" :wrap "M-p (")
  (sp-pair "[" "]" :wrap "M-p [")
  (sp-pair "{" "}" :wrap "M-p {")
  (sp-pair "'" "'" :wrap "M-p '")
  (sp-local-pair 'emacs-lisp-mode "'" "")
  (sp-local-pair 'sh-mode "<" ">" :wrap "M-p <")
  (sp-local-pair 'xml-mode "<" "/>" :wrap "M-p <")
  (sp-local-pair 'latex-mode "$" "$" :wrap "M-p $")
  (sp-local-pair 'org-mode "/" "/" :wrap "M-p /")
  (sp-local-pair 'org-mode "_" "_" :wrap "M-p _")
  )

(use-package multiple-cursors
  :ensure t)

(use-package clean-aindent-mode
  :ensure t
  :config
  (set 'clean-aindent-is-simple-indent t))

;; Package: ws-butler
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

(use-package dictcc
  :ensure t
  :init
  (if window-system
  (define-key input-decode-map [?\C-m] [C-m]))
  :bind (("<C-m> d" . dictcc)
         ("<C-m> D" . dictcc-at-point)))

(use-package winner
  :ensure t
  :init
  (winner-mode)
  :bind (:map winner-mode-map
              ("C-c <left>" . nil)
              ("C-c <right>" . nil)))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-repalce-regexp))
  :config
  (global-anzu-mode))

(use-package recentf
  :ensure t
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 20)
  (recentf-mode)
  )

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
    (add-hook 'flyspell-mode-hook (lambda () (writegood-mode 1)))
    :config
    (set-face-underline 'writegood-passive-voice-face nil)
    (set-face-background 'writegood-duplicates-face "#AA1111")))

(use-package company
  :ensure t
  :bind (("C-." . company-files))
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

(use-package helm
  :ensure t
  :bind (("C-x C-h" . helm-command-prefix)
         ("C-x h" . nil)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x h w" . helm-wikipedia-suggest)
         ("C-x h SPC" . helm-all-mark-rings)
         ("C-x h o" . helm-occur)
         ("C-x h x" . helm-register)
         :map helm-map
         ("C-z" . helm-select-action)
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         :map helm-grep-mode-map
         ("<return>" . helm-grep-mode-jump-other-window)
         ("n" . helm-grep-mode-jump-other-window-forward)
         ("p" . helm-grep-mode-jump-other-window-backward)
         )
  :config
  (defvar helm-alive-p)
  (setq helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source t ; move to end/beginning of source when reaching top/bottom of source.
        helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount 8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  (helm-autoresize-mode t)

  (setq helm-apropos-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (helm-mode 1)
  )

(use-package helm-swoop
  :ensure t
  :after (helm)
  :bind  (("M-s" . helm-swoop)))

(use-package helm-flycheck
  :ensure t
  :after (helm flycheck)
  )
(use-package helm-flyspell
  :ensure t
  :after (helm flyspell)
  )
(use-package helm-company
  :ensure t
  :after (helm company)
  :bind (("C-<tab>" . helm-company)))

(if (locate-file "git" exec-path)
    (use-package magit
      :ensure t
      :bind (( "C-x g" . magit-status))))

(use-package hydra
  :ensure t
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))

;;; Use irony for completion
(if (locate-file "gcc" exec-path)
    (progn
      (use-package irony
        :ensure t
        :config
        (progn
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
      (use-package rtags
        :ensure t
        :init
        (global-unset-key (kbd "M-r"))
        :bind (("M-r d" . rtags-find-symbol-at-point)
               ("M-r f" . rtags-find-symbol)
               ("M-r <left>" . rtags-location-stack-back)
               ("M-r <right>" . rtags-location-stack-forward)
               ("M-r l" . rtags-taglist)
               ("M-r r" . rtags-rename-symbol)
               ("M-r p" . rtags-reparse-file))
        :config
        (progn
          (use-package company-rtags
            :ensure t
            :config
            (add-to-list 'company-backends 'company-rtags)
            )
          (use-package helm-rtags
            :ensure t
            :config
            (setq rtags-display-result-backend 'helm)
            )
          ;; Flycheck setup
          (use-package flycheck-rtags
            :ensure t
            )
          )
        )))

(if (locate-file "gcc" exec-path)
    (progn
      (use-package cmake-mode
        :ensure t)
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
        (rtags-start-process-unless-running)
        (irony-mode)
        (flycheck-select-checker 'rtags)
        ;; RTags creates more accurate overlays.
        (setq-local flycheck-highlighting-mode nil)
        (setq-local flycheck-check-syntax-automatically nil))


      (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

      (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
      (add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

      ))

(use-package ess
  :ensure t
  :config
  (use-package ess-smart-underscore
    :ensure t)
  )

(add-hook 'R-mode-hook #'rainbow-delimiters-mode)
(add-hook 'R-mode-hook #'rainbow-mode)
(add-hook 'R-mode-hook 'hs-minor-mode)

(if (locate-file "julia" exec-path)
    (progn
      (use-package julia-mode
        :ensure t)
      (use-package flycheck-julia
        :ensure t)
      (use-package julia-shell
        :ensure t)
      (add-to-list 'auto-mode-alist '("\\.jl$" . ess-julia-mode))
      (add-hook 'ess-julia-mode-hook #'rainbow-delimiters-mode)
      (add-hook 'ess-julia-mode-hook 'hs-minor-mode)
      (add-hook 'ess-julia-mode-hook 'flycheck-mode)
      (add-to-list 'hs-special-modes-alist '(sh-mode "\\(function\\|while\\|for\\|if\\)" "\\(end\\)" "/[*/]" nil nil))))

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-to-list 'auto-mode-alist '("\\.el$" . lisp-interaction-mode))
(add-hook 'lisp-interaction-mode 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode 'hs-minor-mode)

(if (locate-file "sbcl" exec-path)
    (progn
      (use-package slime
        :ensure t
        :config
        (setq inferior-lisp-program "/usr/bin/sbcl")
        )))

(if (locate-file "gnuplot" exec-path)
    (progn
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
          ))))

(add-hook 'shell-script-mode-hook #'rainbow-delimiters-mode)
(add-hook 'shell-script-mode-hook #'rainbow-mode)
(add-hook 'sh-mode-hook #'rainbow-delimiters-mode)
(add-hook 'sh-mode-hook #'rainbow-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist '(sh-mode "\\(do\\|then\\|in\\)" "\\(done\\|fi\\|esac\\|elif\\)" "/[*/]" nil nil))

(if (locate-file "python" exec-path)
    (progn
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
        (add-hook 'python-mode-hook 'global-ede-mode)
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
                                   ha/elpy-goto-definition)))))

(if (locate-file "xelatex" exec-path)
    (progn
      (use-package auctex
        :ensure t
        :mode (("\\.tex\\'" . latex-mode)
               ("\\.sty\\'" . latex-mode))
        :commands (latex-mode LaTeX-mode plain-tex-mode)
        :config
        (use-package company-auctex
          :ensure t
          :config
          (company-auctex-init))
        (use-package company-bibtex
          :ensure t
          :config
          (add-to-list 'company-backends 'company-bibtex))
        (use-package outline-magic
          :ensure t
          :config
          (define-key outline-minor-mode-map (kbd "<backtab>") 'outline-cycle))
        (TeX-add-style-hook
         "latex"
         (lambda ()
           (LaTeX-add-environments
            '("frame" LaTeX-env-contents)))))


      (defun my-latex-mode-hook()
        (TeX-fold-mode 1)
        (hs-minor-mode nil)
        (outline-minor-mode 1)
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
        (setq TeX-auto-save t
              TeX-parse-self t
              TeX-save-query t
              TeX-PDF-mode t
              TeX-engine 'xetex
              latex-run-command "xelatex --shell-escape"
              reftex-plug-into-AUCTeX t)
        (local-unset-key (kbd "$"))
        )

      (add-hook 'latex-mode-hook 'my-latex-mode-hook)
      (add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

      (add-to-list 'TeX-view-program-list '("okular" "okular -p %(outpage) --unique %o"))
      (setq TeX-view-program-selection
            (quote
             (((output-dvi style-pstricks)
               "dvips and gv")
              (output-dvi "xdvi")
              (output-pdf "okular")
              (output-html "xdg-open"))))
      (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) --shell-escape %S%(PDFout)"))))
      ))

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

(use-package csv-mode
  :ensure t)

(if (locate-file "lua" exec-path)
    (progn
      (use-package lua-mode
        :ensure t)
      (use-package flymake-lua
        :ensure t)
      (use-package luarocks
        :ensure t)
      (use-package company-lua
        :ensure t)))

(if (eq system-type 'windows-nt)
    (setq org-directory "C:/zieglemc/Stuff/ToDo")
  (setq org-directory "/home/zieglemc/Stuff/ToDo"))

(define-obsolete-function-alias 'org-define-error 'define-error)
(defun org-file-path (filename)
  "Return the absolute adress of an org file, given its relative name"
  (interactive)
  (message "%s" (concat (file-name-as-directory org-directory) filename))
  )

(use-package org
  :ensure org-plus-contrib
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
  (rainbow-delimiters-mode 0)
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

(add-to-list 'org-modules 'org-collector)

(defun mz/org-property-sum (prop)
  "Add up all the TALLY properties of headings underneath the current one
     The total is written to the TALLY_SUM property of this heading"
  (interactive "sProperty: ")
  (let ((total 0))
    (save-excursion
      (org-map-tree
       (lambda ()
         (let ((n (org-entry-get (point) prop)))
           (when (stringp n)
             (setq total (+ total (string-to-number n))))))))
    (number-to-string total)))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t) (ruby . t) (gnuplot . t) (python . t) (gnuplot . t) (shell . t) (org . t) (lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)

(setq org-export-coding-system 'utf-8)

(use-package ox-reveal
  :ensure t)
(use-package ox-twbs
  :ensure t)

(if (locate-file "xelatex" exec-path)
    (use-package ox-pandoc
      :ensure t
      :config
      (setq org-pandoc-options-for-docx '((standalone . nil)))
      ))
(use-package org-ref
  :ensure t)


(if (eq system-type 'gnu/linux)
    (progn (setq reftex-default-bibliography '("~/Documents/Literature/bibliography.bib"))

           (setq org-ref-bibliography-notes "~/Documents/Literature/Papers.org"
                 org-ref-default-bibliography '("~/Documents/Literature/bibliography.bib")
                 org-ref-pdf-directory "~/Documents/Literature/bibtex-pdfs/")

           (setq bibtex-completion-bibliography "~/Documents/Literature/bibliography.bib"
                 bibtex-completion-library-path "~/Documents/Literature/bibtex-pdfs/"
                 bibtex-completion-notes-path "~/Documents/Literature/helm-bibtex-notes"))
  (progn (setq reftex-default-bibliography '("C:/zieglemc/24Documents/Literature/bibliography.bib"))

         (setq org-ref-bibliography-notes "C:/zieglemc/24Documents/Literature/Papers.org"
               org-ref-default-bibliography '("C:/zieglemc/24Documents/Literature/bibliography.bib")
               org-ref-pdf-directory "C:/zieglemc/24Documents/Literature/bibtex-pdfs/")

         (setq bibtex-completion-bibliography "C:/zieglemc/24Documents/Literature/bibliography.bib"
               bibtex-completion-library-path "C:/zieglemc/24Documents/Literature/bibtex-pdfs/"
               bibtex-completion-notes-path "C:/zieglemc/24Documents/Literature/helm-bibtex-notes")))

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
(setq org-drill-learn-fraction 0.15)
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

(defhydra hydra-window-stuff ()
  "
                         Split: _v_ert  _s_:horz
                        Delete: _c_lose  _o_nly
                        Winner: _u_ndo  _r_edo
                 Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
                       Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
                        Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
                          Move: _a_:up  _z_:down  _g_oto  _i_menu
"

  ("z" scroll-up-line)
  ("a" scroll-down-line)
  ("g" goto-line)
  ("i" idomenu)

  ("u" winner-undo)
  ("r" winner-redo)

  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("<left>" windmove-left :exit t)
  ("<right>" windmove-right :exit t)
  ("<up>" windmove-up :exit t)
  ("<down>" windmove-down :exit t)

  ("p" mz/previous-buffer)
  ("n" mz/next-buffer)
  ("b" helm-mini)
  ("f" helm-find-file)
  ("F" projectile-find-file)

  ("s" split-window-below)
  ("v" split-window-right)

  ("c" delete-window)
  ("o" delete-other-windows)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)

  ("q" nil))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("q" nil))

(defhydra hydra-hs (:idle 1.0)
  "
               Hide^^            ^Show^          ^Toggle^      ^Navigation^
               ----------------------------------------------------------------
               _h_ hide all      _s_ show all    _t_oggle      _n_ext line
               _d_ hide block    _a_ show block  _TAB_toggle   _p_revious line
               _l_ hide level

               _SPC_ cancel
               "
  ("s" hs-show-all)
  ("h" hs-hide-all)
  ("a" hs-show-block)
  ("d" hs-hide-block)
  ("t" hs-toggle-hiding)
  ("TAB" hs-toggle-hiding :exit t)
  ("l" hs-hide-level)
  ("n" forward-line)
  ("p" (forward-line -1))
  ("SPC" nil)
  )

(defhydra hydra-multiple-cursors ()
  "
               ^Up^            ^Down^        ^Miscellaneous^
          ----------------------------------------------
          [_p_]   Next    [_n_]   Next    [_l_] Edit lines
          [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
          [_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit
       "
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))

(defhydra hydra-org (:color red :hint nil)
  "
          Navigation^
          ---------------------------------------------------------
          _j_ next heading
          _k_ prev heading
          _h_ next heading (same level)
          _l_ prev heading (same level)
          _u_p higher heading
          _<tab>_ Cycle visibility
          _g_o to
          "
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("<tab>" org-cycle)
  ("g" org-goto :exit t))


(defhydra smartparens-hydra ()
  "
       ^LevelMovement^          ^Movement^      ^ParensMovement^
       --------------------------------------------
       [_d_] LevelDown        [_f_] Forward      [_<left>_] BarfLeft
       [_a_] BackLevelUp      [_b_] Back         [_<right>_] BarfRight
       [_w_] LevelUp          [_n_] Next         [_C-<left>_] SlurpLeft
       [_s_] BackLevelDown    [_t_] Transpose    [_C-<right>_] SlurpRight

       [_k_] Kill     [_q_] Quit
     "
  ("d" sp-down-sexp)
  ("w" sp-up-sexp)
  ("a" sp-backward-up-sexp)
  ("s" sp-backward-down-sexp)

  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("t"  sp-transpose-sexp)
  ("n"  sp-next-sexp)

  ("<left>" sp-backward-barf-sexp)
  ("<right>" sp-forward-barf-sexp)
  ("C-<left>" sp-backward-slurp-sexp)
  ("C-<right>" sp-forward-slurp-sexp)

  ("k" sp-kill-sexp "Kill" :color blue)
  ("q" nil "Quit" :color blue))

(global-set-key (kbd "<f12>") 'eval-buffer)
(global-set-key (kbd "<f5>") 'mz/my_compile)
(global-set-key (kbd "M-+") 'mz/fast-calc)
(global-set-key (kbd "M-o") 'mz/new-line-above)
(global-set-key (kbd "C-x \\") 'mz/indent-buffer)

(global-unset-key (kbd "C-x <left>"))
(global-unset-key (kbd "C-x <right>"))
(global-set-key (kbd "C-x <left>") 'mz/previous-buffer)
(global-set-key (kbd "C-x <right>") 'mz/next-buffer)

(global-set-key (kbd "C-<return>") 'make_newline)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-!") 'repeat)

;; ibuffer
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; hydras
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'hydra-hs/body)
(global-set-key (kbd "M-n") 'hydra-multiple-cursors/body)
(global-set-key (kbd "M-P") 'smartparens-hydra/body)
(global-set-key (kbd "M-g") 'hydra-window-stuff/body)
(define-key org-mode-map (kbd "C-c h") 'hydra-org/body)

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

(define-key org-mode-map (kbd "C-c C-r") nil)
(define-key org-mode-map (kbd "C-c C-r b") 'org-ref-helm-insert-cite-link)
(define-key org-mode-map (kbd "C-c C-r r") 'org-ref-helm-insert-ref-link)

(if window-system
    (progn
      (define-key input-decode-map [?\C-m] [C-m])
      (define-key input-decode-map [?\C-i] [C-i])))

(if (file-exists-p "~/PATIENTS/PatDB.el")
    (load-file "~/PATIENTS/PatDB.el")
  )
