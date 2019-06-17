(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(package-initialize)

(abbrev-mode -1)

(defvar my-packages
  '(clojure-mode
    cider
    rainbow-delimiters
    slime
    auctex
    paredit
    color-theme-modern
    magit
    fill-column-indicator
    dockerfile-mode
    yaml-mode
    markdown-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'color-theme)
(load-theme 'charcoal-black t)
(push '(cursor-color . "#00ffff") default-frame-alist)
(push '(background-color . "#001a1a") default-frame-alist)
(push '(font . "Inconsolata 15") default-frame-alist)
(set-face-background 'default "#001a1a")
(set-frame-font "Inconsolata 15" t t)
(set-face-attribute 'region nil :background "#99ffcc" :foreground "#000000")

(require 'fill-column-indicator)
(setf fci-rule-column 79)
(setf fci-rule-width 1)
(setf fci-rule-color "#00ffff")
(setf fci-rule-character ?|)
(setf fci-rule-character-color "#00ffff")
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if buffer-file-name (fci-mode 1))))
(global-fci-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(require 'paredit)
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

(setq inhibit-startup-screen t)

(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message "ssk")

(setq scroll-step 1
      scroll-conservatively 10000)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (toggle-scroll-bar -1)))

(setq column-number-mode t)

(setq c-default-style "linux")

(require 'python)
(setq python-shell-interpreter "python3.5")

(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(require 'slime)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(setq slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
(add-hook 'slime-mode-hook #'enable-paredit-mode)
(add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

(require 'clojure-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'cider)
(setf cider-repl-display-help-banner nil)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(require 'tramp)
(setq tramp-default-method "ssh")
(put 'downcase-region 'disabled nil)
