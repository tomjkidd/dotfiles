;;; init.el --- My cobbled together emacs config

;;; Commentary:

;; Started this by following along with...
;; http://cestlaz.github.io/posts/using-emacs-1-setup/#.WX0dGtPyto4
;; It has evolved to include other things I commonly use

;; Basic Editor Config
;; ===================

;;; Code:

(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(global-linum-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)
(blink-cursor-mode -1)
(global-prettify-symbols-mode -1)
(global-hl-line-mode +1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode)
; 'expression | 'parenthesis
(setq show-paren-style 'parenthesis)
(setq electric-indent-mode nil)
;; %b - buffername | %f - filepath
(setq-default frame-title-format "%b")

; C-h v whitespace-style
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing spaces empty indentation::space space-before-tab space-before-tab::tab space-mark))
;(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Window Keys
;; ===========

;; Move between visible buffers
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "s-w") 'delete-window)

(global-set-key (kbd "M-s-^") 'windmove-up)   ;; M-s-i
(global-set-key (kbd "M-s-˚") 'windmove-down) ;; M-s-k
(global-set-key (kbd "M-s-∆") 'windmove-left) ;; M-s-j
(global-set-key (kbd "M-s-¬") 'windmove-right);; M-s-l

;; s-w will kill current buffer, s-e is close and will try to kill a bunch
(global-set-key (kbd "s-e") 'kill-some-buffers)

;; Unbind the window minimizing behavior (suspend-frame)
(global-unset-key (kbd "C-z"))

;; Package Setup
;; =============

(require 'package)
;; Disables automatic package loading, see `Packaging Basics`
(setq package-enable-at-startup nil)
;; see `Creating and Maintaining Package Archives`
;; Form is (<id> . <location>)
;; <id> and <location> are strings, <location> can be an http link or dir
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; See `Packaging Basics`
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Emacs tutorial packages
;; =======================

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ace-window
  :ensure t
  :init
  ;; When lots of windows open, use number keys to nav
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;; ido - interactively do
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)

;; window management...
(winner-mode -1)
;;(windmove-default-keybindings)

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  ;; NOTE: When using M-x to rgrep, it's useful to know 'ivy-immediate-done.
  ;; It will allow you to specify *.clj* as a raw input, without having to use a completion
  ;; It is normally mapped to C-u C-j, but there was a suggestion that I liked here
  ;; https://github.com/abo-abo/swiper/issues/55
  (ivy-mode 1)
  (setq ivy-wrap nil)
  (setq ivy-height 10)
  (setq ivy-use-virtual-buffers t)
  (setq confirm-nonexistent-file-or-buffer t)
  (setq ivy-extra-directories nil)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "<C-return>") 'ivy-immediate-done)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package projectile
  :disabled)

(use-package avy
  :ensure t
  ;; avy-goto-char maps every occurence of a typed char to a series of keys.
  ;; Typing the matching series will nav to a specific area in the visible buffer.
  ;; In a clojure file, `M-s (` is useful to get to the start of a visible form
  :bind ("C-q" . avy-goto-word-1))

(use-package auto-complete
  :ensure t
  :init
  ;; Will do auto-complete based on words already in the buffer
  ;; M-n and M-p move through auto-complete options
  ;; C-i and C-m to move through and select
  (ac-config-default)
  (global-auto-complete-mode t))

;; To use org-mode to condense a large config file...
;; In org mode type <s-<Tab> and add a emacs-lisp line, then put list in the
;; block that gets created.
;; This is overkill for what I'm up to at the moment...
;;(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; Theme Packages
;; ==============

(use-package powerline
  :ensure t)

(use-package moe-theme
  :ensure t
  :config
  (require 'powerline)
  ;(moe-dark) ;; I don't like how this affects switcher
  ;(moe-theme-set-color 'cyan)
  ;(set-face-attribute 'powerline-active2 nil :background "#4e4e4e" :foreground "#3a3a3a")
  ;(set-face-attribute 'powerline-inactive2 nil :background "#4e4e4e" :foreground "#4e4e4e")
  (powerline-moe-theme))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (setq monokai-height-minus-1 0.8
	  monokai-height-plus-1 1.1
	  monokai-height-plus-2 1.15
	  monokai-height-plus-3 1.2
	  monokai-height-plus-4 1.5))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Editor Packages
;; ===============

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  ;; Useful for completions, used to be what I used for M-x
  (global-set-key (kbd "M-z") 'smex))

(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-menu-items 40)
  (defun ido-choose-from-recentf ()
    "Use ido to select a recently visited file from the `recentf-list'"
    (interactive)
    (find-file (ido-completing-read "Open file: " recentf-list nil t)))

  ;; bind it to "C-c f"
  (global-set-key (kbd "C-c f") 'ido-choose-from-recentf))

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places"))
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))
  (setq auto-save-default  nil))

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-list-symbol))

(use-package speedbar
  :ensure t)

(use-package sr-speedbar
  :ensure t
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-use-images nil)
  (setq sr-speedbar-right-side nil)
  (global-set-key (kbd "C-c n") 'sr-speedbar-toggle))

;; TODO: checkout ido-ubiquitous package

;; Clojure Packages
;; ================

(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package magit
  :ensure t
  :config
  ;; The following keybinding brings you to a git status window
  ;; From there s/u can be used to state/unstage
  ;; l can be used to show log
  ;; c can be used to commit
  ;; d can be used to diff
  ;; h can be used to create branches
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package rainbow-mode
  :ensure t
  ;; css colors display inline
  )

(use-package markdown-mode
  :ensure t)

(moe-theme-set-color 'cyan)

;; Experimental
;; ============

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  ;; Ran `gem install sqlint` to get this exe
  ;; C-c ! v - Go to menu to enable stuff
  (setq flycheck-sql-sqlint-executable "/usr/local/bin/sqlint"))

(use-package clojure-snippets
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand))

;; Found on sacha chua's blog, more useful to me than normal C-a
(defun sacha/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'sacha/smarter-move-beginning-of-line)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-color "#FF0000"))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode 1)
  ;(setq beacon-color "#FF0000")
  )

(use-package expand-region
  :ensure t
  :config
  ;; This finally provides a nice way to select and entire s-expr!
  ;; If you are at the starting paren, C-= will select up to the
  ;; matching closing paren. This makes quick C-w for yanking an
  ;; s-expr to move it, tasty!
  (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode clojure-snippets flycheck rainbow-mode magit cider clojure-mode rainbow-delimiters paredit sr-speedbar smex monokai-theme moe-theme powerline auto-complete counsel ivy-hydra ace-window org-bullets which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
