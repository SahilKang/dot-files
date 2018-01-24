(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

(show-paren-mode 1)

(setq show-paren-sytle 'expression) ;highlight entire expression

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-charcoal-black)))

(setq inhibit-startup-screen t)

(setq initial-scratch-message ";; Welcome back, Sahil.\n\n")
(setq inhibit-startup-echo-area-message "ssk")

(setq scroll-step 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

(setq column-number-mode t)

(setq c-default-style "linux")

(defvar my-packages
  '(
    clojure-mode
    cider
    rainbow-delimiters
    slime
    auctex))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'python)
(setq python-shell-interpreter "python3.5")

(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(require 'slime)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(add-hook 'slime-mode-hook #'enable-paredit-mode)
(add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
