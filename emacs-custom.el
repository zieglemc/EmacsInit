(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "306b5b974f90568474442be4aa47a8f3c74b40def1f51430a006ce420a7c32b7" default)))
 '(package-selected-packages
   (quote
    (benchmark-init circe circe-notifications smart-mode-line smart-mode-line-powerline-theme airline-themes anaconda-mode emacs-xkcd flycheck function-args helm-c-yasnippet helm-dictionary helm-make helm-mt helm-swoop highlight-symbol jedi multi-term rtags speed-type web-mode rainbow-mode rainbow-delimiters htmlize org-ac magit magit-popup flycheck-clangcheck flycheck-cstyle flycheck-irony flymake flymake-python-pyflakes ws-butler volatile-highlights undo-tree smartparens projectile-codesearch paredit ox-twbs ox-reveal ox-html5slide org-projectile org-bullets mc-extras matlab-mode linum-relative latex-unicode-math-mode latex-preview-pane latex-pretty-symbols latex-math-preview latex-extra julia-shell indent-guide iedit hlinum highlight-parentheses highlight-blocks highlight helm-systemd helm-projectile helm-package helm-org-rifle helm-gtags helm-google helm-git helm-flyspell helm-flymake helm-flycheck helm-company helm-bibtex helm-R gnuplot-mode gnuplot flylisp ess-smart-underscore ess-smart-equals ess-R-object-popup ess-R-data-view dtrt-indent company-shell company-math company-irony-c-headers company-irony company-cmake company-c-headers company-auctex cmake-project cmake-mode cmake-ide clean-aindent-mode auto-dictionary anzu)))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "lawn green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "medium spring green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "saddle brown"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "goldenrod")))))
