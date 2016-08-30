
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("306b5b974f90568474442be4aa47a8f3c74b40def1f51430a006ce420a7c32b7" default)))
 '(package-selected-packages
   (quote
    (htmlize org-ac magit magit-popup flycheck-clangcheck flycheck-cstyle flycheck-irony flymake flymake-python-pyflakes ws-butler volatile-highlights undo-tree smartparens projectile-codesearch paredit ox-twbs ox-reveal ox-html5slide org-projectile org-bullets mc-extras matlab-mode linum-relative latex-unicode-math-mode latex-preview-pane latex-pretty-symbols latex-math-preview latex-extra julia-shell indent-guide iedit hlinum highlight-parentheses highlight-blocks highlight helm-systemd helm-projectile helm-package helm-org-rifle helm-gtags helm-google helm-git helm-flyspell helm-flymake helm-flycheck helm-company helm-bibtex helm-R gnuplot-mode gnuplot flylisp ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view dtrt-indent company-shell company-math company-irony-c-headers company-irony company-cmake company-c-headers company-auctex cmake-project cmake-mode cmake-ide clean-aindent-mode auto-dictionary anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
