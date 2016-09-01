(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq user-full-name "Marc Ziegler"
      user-email-adress "marc.ziegler@uk-erlangen.de")

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq gc-cons-threshold 100000000)
(defalias 'yes-or-no-p 'y-or-n-p)
(winner-mode t)

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 2)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(display-time-mode t)
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

(defun emacs-reload()
  "Reload the emacs ini file (~/.emacs.d/init.el)"
  (interactive)
  (load-file '"~/.emacs.d/init.el")
)

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (point-to-register 'o)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (jump-to-register 'o)
)

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(require 'smartparens)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-recursive t)

(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)
(semantic-add-system-include "/usr/include/itk" 'c++-mode)
(semantic-mode 1)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-irony)
(global-company-mode 1)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)

(require 'undo-tree)
(global-undo-tree-mode)

;; GROUP: Editing -> Matching -> Isearch -> Anzu
(require 'anzu)
(global-anzu-mode)

(require 'yasnippet)
(yas-global-mode 1)

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(require 'helm)
(require 'helm-config)
(require 'helm-google)
(require 'helm-flycheck)
(require 'helm-flyspell)
(require 'helm-company)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-autoresize-mode t)

(setq helm-apropos-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


;; Enable helm-gtags-mode
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(require 'helm-grep)

(helm-mode 1)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(require 'magit)

;; setup GDB
(setq gdb-many-windows t ;; use gdb-many-windows by default
      gdb-show-main t  ;; Non-nil means display source file containing the main routine at startup
)
(setq
 c-default-style "linux"
)
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode and related modes
  (require 'ede)
  (global-ede-mode)
  (hs-minor-mode)
  (setq flycheck-checker 'c/c++-gcc)
  (flycheck-mode)
  (turn-on-auto-fill)
  (global-set-key [f6] 'run-cfile)
  (global-set-key [C-c C-y] 'uncomment-region)
  (irony-mode)
)

(add-hook 'c-mode-common-hook   'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'matlab-mode-hook 'auto-complete-mode)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode))

(add-to-list 'auto-mode-alist '("\\.el$" . lisp-mode))

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(add-to-list 'auto-mode-alist '("\\.gnu$" . gnuplot-mode))
(add-to-list 'auto-mode-alist '("\\.plt$" . gnuplot-mode))

(add-hook 'gnuplot-mode-hook
          (lambda () (local-set-key (kbd "C-c C-c") 'gnuplot-run-buffer)))

(require 'company-auctex)
(company-auctex-init)
(setq-default TeX-engine 'xetex)
(setq latex-run-command "xelatex --shell-escape")
(setq-default TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'TeX-mode-hook
          (lambda ()
      (flyspell-mode 1)
            (TeX-fold-mode 1)
            (add-hook 'find-file-hook 'TeX-fold-buffer t t)
      (local-set-key [C-tab] 'TeX-complete-symbol)
      (local-set-key [C-c C-g] 'TeX-kill-job)
      )
    )

(add-to-list 'auto-mode-alist '("\\.tex$" . TeX-mode))
(add-to-list 'auto-mode-alist '("\\.sty$" . TeX-mode))

(TeX-add-style-hook
 "latex"
 (lambda ()
   (LaTeX-add-environments
    '("frame" LaTeX-env-contents))))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook
      (lambda()
        (local-set-key [C-tab] 'TeX-complete-symbol)))
(require 'auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))

(setq TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
(setq LaTeX-command-style (quote (("" "%(PDF)%(latex) --shell-escape %S%(PDFout)"))))

(require 'ox-reveal)
(require 'ox-twbs)
(require 'org-contacts)
(setq org-directory "/home/zieglemc/Stuff/ToDo")

(defun org-file-path (filename)
  "Return the absolute adress of an org file, given its relative name"
  (interactive)
  (concat (file-name-as-directory org-directory) filename)
  )

(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s" ))

(setq org-reveal-root "file:///home/zieglemc/src/reveal.js-master/js/reveal.js")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.todo$" . org-mode))

(setq org-hide-leading-stars t)
(setq org-ellipsis " ↷")  
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-agenda-custom-commands
      '(("W" agenda "" ((org-agenda-ndays 21)))))

(setq org-agenda-files (quote ("~/Stuff/ToDo/agenda.org" "~/Stuff/ToDo/worktime.org" "~/Stuff/ToDo/todo.org" "~/Stuff/ToDo/ideas.org" "~/Stuff/ToDo/to-read.org")))

(setq org-agenda-files `(
          ,(org-file-path "worktime.org")
          ,(org-file-path "todo.org")
          ,(org-file-path "ideas.org") 
          ,(org-file-path "to-read.org")
          ,(org-file-path "agenda.org")))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)

(setq org-log-done 'time)

(org-babel-do-load-languages 'org-babel-load-languages 
                             '((emacs-lisp . t) (ruby . t) (gnuplot . t) ))
(setq org-confirm-babel-evaluate nil)

(setq org-capture-templates
      '(
        ("t" "Todo"
         entry
         (file (org-file-path "todo.org")))
        ("i" "Ideas"
         entry
         (file (org-file-path "ideas.org")))
        ("r" "To Read"
         checkitem
         (file (org-file-path "to-read.org")))
        ))

;; PACKAGE: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(global-set-key (kbd "M-o") 'prelude-smart-open-line)

(global-set-key (kbd "<f12>") 'eval-buffer)
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))
(global-set-key [C-c C-y] 'uncomment-region)

(fset 'make_newline
      [?\C-e tab return])

(global-set-key (kbd "C-<return>") 'make_newline)

(global-set-key "\C-x\\" 'indent-buffer)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

(global-set-key (kbd "C-<tab>") 'company-complete)
(define-key global-map (kbd "C-.") 'company-files)

(global-set-key (kbd "C-!") 'repeat)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-g <left>") 'windmove-left)
(global-set-key (kbd "M-g <right>") 'windmove-right)
(global-set-key (kbd "M-g <up>") 'windmove-up)
(global-set-key (kbd "M-g <down>") 'windmove-down)

(global-set-key (kbd "C-x g") 'magit-status)

(define-key winner-mode-map (kbd "C-c <left>") nil)
(define-key winner-mode-map (kbd "C-c <right>") nil)

(global-set-key (kbd "M-g <prior>") 'winner-undo)
(global-set-key (kbd "M-g <next>") 'winner-redo)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c <left>") 'org-metaleft)
(global-set-key (kbd "C-c <right>") 'org-metaright)
(global-set-key (kbd "C-c <up>") 'org-metaup)
(global-set-key (kbd "C-c <down>") 'org-metadown)
(global-set-key (kbd "C-c S-<left>") 'org-metashiftleft)
(global-set-key (kbd "C-c S-<right>") 'org-metashiftright)
(global-set-key (kbd "C-c S-<up>") 'org-metashiftup)
(global-set-key (kbd "C-c S-<down>") 'org-metashiftdown)

(global-set-key (kbd "<f10>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-step);; equiv matlab step in
(global-set-key (kbd "<f8>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out
