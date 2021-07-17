(add-to-list 'package-archives '("stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

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
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)

(setq global-mark-ring-max 5000)
(setq mark-ring-max 5000)
(delete-selection-mode)

(setq visible-bell 1)

(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
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
;; (setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line, because it's mostly
;;       ;; invisible here anyway.
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))
(winner-mode)

(load-theme 'CrapCram t)

(if (eq system-type 'windows-nt)
    (set-face-font 'default "-outline-Fira Code-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
  (set-face-font 'default "-CTDB-Fira Code-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1"))

(set-face-attribute 'default nil :height 95)

(use-package ligature
  :load-path (lambda () (concat user-emacs-directory "ligature"))
  :config
  ;;   ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www" "->" "-->" "<|" "|>"))
  ;;   ;; Enable traditional ligature support in eww-mode, if the
  ;;   ;; `variable-pitch' face supports it
  ;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;;   ;; Enable all Cascadia Code ligatures in programming modes
  ;;   ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

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

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode 1)
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 15)
  )

(use-package all-the-icons
  :ensure t
  )

(use-package dashboard
  :ensure t
  :preface
  (defun mz/dashboard-banner ()
    """Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done)))

  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry (lambda () (when (org-entry-is-done-p) (point))))
  (setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  (setq dashboard-footer-icon (all-the-icons-fileicon "emacs"
                                                      :height 1.1
                                                      :v-adjust -0.05
                                                      :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook))

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
  (setq star-buffer-exceptions '("^\\*scratch\\*$" "^\\*R.*\\*$" "^\\*Python.*\\*$"
                                 "^\\*julia.*\\*$" "^\\*shell\\*$") )
  (setq normal-buffer-exceptions '("^magit[-:].*$"))
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
  (sp-local-pair 'org-mode "_" "_" :wrap "M-p _"))

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(use-package multiple-cursors
  :ensure t)

(use-package recentf
  :ensure t
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 20)
  (recentf-mode))

(use-package flyspell
  :ensure t)

(use-package writegood-mode
  :ensure t
  :init
  (add-hook 'flyspell-mode-hook (lambda () (writegood-mode 1)))
  :config
  (set-face-underline 'writegood-passive-voice-face nil)
  (set-face-background 'writegood-duplicates-face "#AA1111"))

;; (use-package dictcc
;;   :ensure t
;;   :init
;;   (if window-system
;;       (define-key input-decode-map [?\C-m] [C-m]))
;;   :bind (("<C-m> d" . dictcc)
;;          ("<C-m> D" . dictcc-at-point)))

(use-package company
  :ensure t
  :bind (("C-." . company-files))
  :config
  (setq company-frontends nil)
  (add-to-list 'company-backends 'company-elisp)
  (add-to-list 'company-backends 'company-capf)
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
  :bind (("C-<tab>" . helm-company))
  :init (progn
          (defun my-helm-company-complete ()
            (interactive)
            (when (company-complete) (helm-company)))
          (add-to-list 'completion-at-point-functions
                       #'comint-dynamic-complete-filename)))

(if (or (locate-file "git" exec-path) (locate-file "git.exe" exec-path))
    (use-package magit
      :ensure t
      :bind (( "C-x g" . magit-status))))

;;     (use-package evil
;;       :ensure t
;;       :init
;;       (setq evil-want-integration t)
;;       (setq evil-want-keybinding nil)
;;       (setq evil-want-C-u-scroll t)
;;       :config
;;       (evil-mode 1)
;;       )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . company-mode)
  :config
  (use-package helm-lsp
    :ensure t
    :commands helm-lsp-workspace-symbol)
  )

(use-package ess
  :ensure t
  :config
  ;;:hook (R-mode . lsp-mode)
  (use-package ess-smart-underscore
    :ensure t)
  )

(add-hook 'R-mode-hook #'rainbow-delimiters-mode)
(add-hook 'R-mode-hook #'rainbow-mode)
(add-hook 'R-mode-hook 'hs-minor-mode)
(add-hook 'R-mode-hook 'lsp-mode)

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
      (add-hook 'julia-mode-hook 'hs-minor-mode)
      (add-hook 'ess-julia-mode-hook 'flycheck-mode)
      (add-to-list 'hs-special-modes-alist
                   '(julia-mode "\\(function*\\|while*\\|for*\\|if*\\)" "\\(end\\)" "/[*/]" forward-sexp hs-c-like-adjust-block-beginning))
      (add-to-list 'hs-special-modes-alist
                   '(ess-julia-mode "\\(function*\\|while*\\|for*\\|if*\\)" "\\(end\\)" "/[*/]" forward-sexp hs-c-like-adjust-block-beginning))))

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-to-list 'auto-mode-alist '("\\.el$" . lisp-interaction-mode))
(add-hook 'lisp-interaction-mode 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode 'hs-minor-mode)

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

(add-to-list 'auto-mode-alist '("\\.service$" . conf-mode))

(if (locate-file "python" exec-path)
    (progn
      (use-package python
        :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
        :interpreter ("python" . python-mode)
        :hook (python-mode . lsp-deferred)
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
        )))

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

      ;;(add-to-list 'TeX-view-program-list '("okular" "okular -p %(outpage) --unique %o"))
      (setq TeX-view-program-selection
            (quote
             (((output-dvi style-pstricks)
               "dvips and gv")
              (output-dvi "xdvi")
              (output-pdf "okular")
              (output-html "xdg-open"))))
      (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) --shell-escape %S%(PDFout)"))))

      (use-package lsp-latex
        :ensure t
        :hook ((Latex-mode . lsp)
               (latex-mode . lsp)
               (bibtex-mode . lsp))
        )
      )
  )

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
        :ensure t
        :config
        (add-to-list 'company-backends 'company-lua))))

(if (eq system-type 'windows-nt)
    (setq org-directory "C:/ToDo")
  (setq org-directory "/home/zieglemc/ToDo"))

(define-obsolete-function-alias 'org-define-error 'define-error "27.0")
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

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(setq org-hide-leading-stars t)
(setq org-ellipsis "  ↘") ;(format "%s" (all-the-icons-material "wrap_text" :height 1.5)))

(use-package org-bullets
  :ensure t
  )

(use-package org-pretty-tags
  :ensure t
  :config
  (org-pretty-tags-global-mode)
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
                         ,(org-file-path "todo.org")
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

(use-package ox-twbs
  :ensure t)

(if (locate-file "xelatex" exec-path)
    (use-package ox-pandoc
      :ensure t
      :config
      (setq org-pandoc-options-for-docx '((standalone . nil)))
      ))

;;(add-to-list 'org-modules 'org-drill)
;;(setq org-drill-add-random-noise-to-intervals-p t)
;;(setq org-drill-hint-separator "|")
;;(setq org-drill-left-cloze-delimiter "<[")
;;(setq org-drill-right-cloze-delimiter "]>")
;;(setq org-drill-learn-fraction 0.15)
;;(load-file "~/.emacs.d/mz-functions/learnjapanese.el")
;; (setq jp/vocabulary-file (org-file-path "Vocabulary.org"))
;; (add-to-list 'org-capture-templates
;;              '("j" "Japanese Word/Phrase" entry (file+headline jp/vocabulary-file "Words and Phrases")
;;                "** %(jp/type-prompt)     :drill:\n   :PROPERTIES:\n   :DRILL_CARD_TYPE: multisided\n   :ADDED:    %U\n   :END:\n*** Japanese\n    %(jp/japanese-get-word (jp/japanese-prompt))\n*** English\n    %(jp/english-prompt)"))
;; (add-to-list 'org-capture-templates
;;              '("J" "Japanese Grammar" entry (file+headline jp/vocabulary-file "Grammar")
;;                "** %(jp/grammar-type-prompt) :drill:\n   :PROPERTIES:\n   :DRILL_CARD_TYPE: hide2cloze\n   :ADDED:    %U\n   :END:\n   %(jp/definition-prompt)\n*** Example\n    %(jp/japanese-get-word (jp/japanese-prompt))\n    %(jp/english-prompt)"))

(setq mz/todo-file (org-file-path "todo.org"))
;;(setq mz/ideas-file (org-file-path "ideas.org"))
;;(setq mz/to-read-file (org-file-path "to-read.org"))
;;(setq mz/how-to-file (org-file-path "how-to.org"))
(setq mz/agenda-file (org-file-path "agenda.org"))

(setq org-capture-templates
      '(
        ("t" "Todo"
         entry
         (file mz/todo-file))
        ;;("i" "Ideas"
        ;; entry
        ;; (file mz/ideas-file))
        ;;("r" "To Read"
        ;; checkitem
        ;; (file mz/to-read-file))
        ;;("h" "How-To"
        ;; entry
        ;; (file mz/how-to-file))
        ))

(add-to-list 'org-capture-templates
             '("a" "Agenda Entry" entry (file mz/agenda-file)
               "* %^{Appointment}            %^G\n  %^T\n%?"))

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

(define-key winner-mode-map (kbd "C-c <left>") nil)
(define-key winner-mode-map (kbd "C-c <right>") nil)

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
;;(define-key org-mode-map (kbd "C-c C-r b") 'org-ref-helm-insert-cite-link)
;;(define-key org-mode-map (kbd "C-c C-r r") 'org-ref-helm-insert-ref-link)

(if window-system
    (progn
      (define-key input-decode-map [?\C-m] [C-m])
      (define-key input-decode-map [?\C-i] [C-i])))
