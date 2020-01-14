(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
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
    markdown-mode
    all-the-icons
    telephone-line
    doom-themes
    ivy
    swiper))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'doom-themes)
(setq doom-city-lights-brighter-comments t
      doom-city-lights-comment-bg nil)
(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (with-selected-frame frame
	 (load-theme 'doom-city-lights t)
	 (toggle-scroll-bar -1))))
  (load-theme 'doom-city-lights t))

;;(load-theme 'charcoal-black t)
;;(push '(cursor-color . "#00ffff") default-frame-alist)
;;(push '(background-color . "#001a1a") default-frame-alist)
(push '(font . "Inconsolata 15") default-frame-alist)
;;(set-face-background 'default "#001a1a")
(set-frame-font "Inconsolata 15" t t)
;;(set-face-attribute 'region nil :background "#99ffcc" :foreground "#000000")

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

(setq-default indent-tabs-mode nil)

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

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'python)
(setq python-shell-interpreter "python3")

(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(require 'slime)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-mdot-fu))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(setq slime-description-autofocus t)
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
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'telephone-line)

(telephone-line-defsegment %buffer-segment
  mode-line-buffer-identification)

(telephone-line-defsegment %filesize-segment
  "%I")

(defface %teal '((t (:foreground "#001a1a" :background "#99ffcc"))) "" :group 'mode-line)
(defface %onyx '((t (:foreground "#99ffcc" :background "#001a1a"))) "" :group 'mode-line)
(defface %cyan '((t (:foreground "#001a1a" :background "#00ffff"))) "" :group 'mode-line)
(defface %buffer '((t (:foreground "#99ffcc" :background "#001a1a"))) "" :group 'mode-line)

(add-hook
 'read-only-mode-hook
 (lambda ()
   (if buffer-read-only
       (face-remap-set-base
	'%buffer '((:foreground "#000000" :background "#cc99ff")))
     (if (buffer-modified-p)
	 (face-remap-set-base
	  '%buffer '((:foreground "#000000" :background "#ffcc99")))
       (face-remap-reset-base '%buffer)))))

(add-hook
 'first-change-hook
 (lambda ()
   (face-remap-set-base
    '%buffer '((:foreground "#000000" :background "#ffcc99")))))

(add-hook
 'after-revert-hook
 (lambda ()
   (face-remap-reset-base '%buffer)))

(add-hook
 'after-save-hook
 (lambda ()
   (face-remap-reset-base '%buffer)))

(advice-add 'undo :after (lambda (&rest _)
			   (if (buffer-modified-p)
			       (face-remap-set-base
				'%buffer '((:foreground "#000000" :background "#ffcc99")))
			     (face-remap-reset-base '%buffer))))

(setq telephone-line-faces
      '((teal . (%teal . mode-line-inactive))
	(onyx . (%onyx . mode-line-inactive))
	(cyan . (%cyan . mode-line-inactive))
	(buffer . (%buffer . mode-line-inactive))
	(accent . (telephone-line-accent-active . telephone-line-accent-inactive))
	(nil . (mode-line . mode-line-inactive))))

(setq telephone-line-subseparator-faces '((onyx . cyan)))

(setq telephone-line-lhs
      '((buffer . (%buffer-segment))
	(onyx . (telephone-line-airline-position-segment
		 %filesize-segment))))

(setq telephone-line-rhs
      '((onyx . (telephone-line-vc-segment
		 telephone-line-major-mode-segment
		 telephone-line-minor-mode-segment))))

(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-left
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)

(telephone-line-mode t)
;;;
;; (require 'all-the-icons)
;; (all-the-icons-insert-icons-for 'octicon)
;; (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;; (setq inhibit-compacting-font-caches t)
;; (all-the-icons-octicon "rocket")
;;;

(require 'ivy)
(require 'swiper)

(ivy-mode t)
(setq ivy-use-virtual-buffers t
      enable-recursive-minibuffers t
      ivy-count-format "(%d/%d) "
      ivy-use-selectable-prompt t)
(face-spec-set 'ivy-current-match
	       '((((class color) (background dark))
		  :foreground "#99ffcc" :bold t :background nil)))
(global-set-key (kbd "C-s") 'swiper)
;;;
;;;
(setq js-indent-level 2)
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)
