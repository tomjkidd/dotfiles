;;; .emacs --- My cobbled together emacs config

;;; Commentary:

;; Started this by following along with...
;; http://cestlaz.github.io/posts/using-emacs-1-setup/#.WX0dGtPyto4
;; It has evolved to include other things I commonly use

;;; Code:

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

;; Basic Editor Config
;; ===================

;; expand to full-screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; don't show various things
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; don't do distracting things
(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)

;; don't lock files or backup
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; don't allow tab indentation
(setq-default indent-tabs-mode nil)

;; disable electric-indent-mode, I prefer C-j
(setq electric-indent-mode nil)

;; Unbind the window minimizing behavior (suspend-frame)
(global-unset-key (kbd "C-z"))

;; quick y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; format buffer title
;; %b - buffername | %f - filepath
(setq-default frame-title-format "%b")

;; highlight the line the cursor is on
(global-hl-line-mode +1)

;; highlight matching parens
(use-package paren
  :ensure t
  :config
  (show-paren-mode)
  ; 'expression | 'parenthesis
  (setq show-paren-style 'parenthesis))

;; This can be used to configure do things like show the lambda symbol instead of defn
;; don't worry about that for now.
(global-prettify-symbols-mode -1)

;; show whitespace in a sane manner, clean up on save
(use-package whitespace
  :ensure t
  :config
  ; C-h v whitespace-style
  (global-whitespace-mode 1)
  (setq whitespace-style '(face trailing spaces empty indentation::space space-before-tab space-before-tab::tab space-mark))
  ;(add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; ido - interactively do
;; TODO: checkout ido-ubiquitous package
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

;; configure isearch so that it still has keybindings
(global-set-key (kbd "s-s") 'isearch-forward-regexp)
(global-set-key (kbd "s-r") 'isearch-backward-regexp)

;; configure hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-list-symbol))

(defalias 'list-buffers 'ibuffer)

;; enable to keep track of window orginization for undo
(winner-mode -1)

;; use shift and arrow keys to move between buffers
;; I prefer M-s and arrow keys, to work like Chrome tabs
;;(windmove-default-keybindings)

;; To use org-mode to condense a large config file...
;; In org mode type <s-<Tab> and add a emacs-lisp line, then put list in the
;; block that gets created.
;; This is overkill for what I'm up to at the moment...
;;(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; allow emacs to find programs on $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; git integration
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

;; displays a list of emacs commands that have been executed using C-c o
(use-package command-log-mode
  :ensure t
  :config
  (global-command-log-mode t)
  ;; NOTE: uncomment the following line to log even trivial commands
  ;; This is useful for pairing and showing everything that happens
  ;;(setq clm/log-command-exceptions* '(nil))
  )

;; show available commands when a prefix is typed
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Dropbox/org/agenda.org"
                           "~/Dropbox/org/exocortex.org"
                           "~/secrets/org/agenda.org")))

;; Language Modes
;; ==============

(use-package clojure-mode
  :ensure t
  :config
  ;; select region and hit tab to do clojure-align
  (setq clojure-align-forms-automatically t)

  ;; M-r paredit-raise-sexp will turn (some? [a b c]) to [a b c]
  ;; Note: In previous case, cursor is on '[' of [a b c]

  ;; C-M-u backward-up-list
  ;; C-M-d down-list
  (add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (put-clojure-indent 'reg-event-db 1)
            (put-clojure-indent 'reg-event-fx 1)
            (put-clojure-indent 'reg-fx 1)
            (put-clojure-indent 'reg-cofx 1)
            (put-clojure-indent 'reg-sub 1)
            (put-clojure-indent 'bind-relations 1)
            (put-clojure-indent 'react-method 1)
            (put-clojure-indent 'render 1)
            (put-clojure-indent 'initLocalState 1)
            (put-clojure-indent 'componentWillMount 1)
            (put-clojure-indent 'componentDidMount 1)
            (put-clojure-indent 'componentWillReceiveProps 1)
            (put-clojure-indent 'shouldComponentUpdate 1)
            (put-clojure-indent 'componentWillUpdate 1)
            (put-clojure-indent 'componentDidUpdate 1)
            (put-clojure-indent 'componentWillUnmount 1)
            (enable-paredit-mode)
            (subword-mode))))

(use-package markdown-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

;; Flycheck error and linting
;; ==========================
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  ;; Ran `gem install sqlint` to get this exe
  ;; C-c ! v - Go to menu to enable stuff
  (setq flycheck-sql-sqlint-executable "/usr/local/bin/sqlint"))

(use-package flycheck-joker
  :ensure t
  ;; Adds flycheck support for clojure/clojurescript
  ;; Ran `brew install candid82/brew/joker` to get this exe
  )

;; Autocomplete
;; ============
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   ;; Will do auto-complete based on words already in the buffer
;;   ;; M-n and M-p move through auto-complete options
;;   ;; C-i and C-m to move through and select
;;   (ac-config-default)
;;   (global-auto-complete-mode 0)

;;   ;; The following allows ivy-like bindings
;;   ;; C-n -> 'ac-next
;;   ;; C-p -> 'ac-previous
;;   ;; C-j -> 'ac-complete
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map (kbd "C-j") 'ac-complete))

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (global-company-mode t)

  ;; The following allows ivy-like bindings
  ;; C-n -> go to next
  ;; C-p -> go to previous
  ;; C-j -> select current
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "C-j") #'company-complete-selection)))

(use-package company-quickhelp
  :ensure t)

;; Navigation and window/buffer management
;; =======================================

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

;; buffer navigation - use number keys to nav
(use-package ace-window
  :ensure t
  :init
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2.0))))))

;; provide ag (the silver surfer) for fast searches
(use-package ag
  :ensure t
  ;; Ran `brew install the_silver_searcher` to get this exe
)

;; swiper minibuffer short modal keybindings
(use-package ivy-hydra
  :ensure t)

;; completion functions using ivy
(use-package counsel
  :ensure t
  ;;counsel-yank-pop is useful to see the kill-ring history
  )

;; allows many search contexts through a consistent minibuffer
;; TODO: Check on https://github.com/yevgnen/ivy-rich
(use-package swiper
  :ensure t
  :config
  ;; NOTE: When using M-x to rgrep, it's useful to know 'ivy-immediate-done.
  ;; It will allow you to specify *.clj* as a raw input, without having to use a completion
  ;; It is normally mapped to C-u C-j, but there was a suggestion that I liked here
  ;; https://github.com/abo-abo/swiper/issues/55
  ;; ivy mode is also really useful

  ;; When C-c j is used to open git-grep, you can use M-q to do a query/replace matches
  (ivy-mode 1)
  (setq ivy-wrap nil)
  (setq ivy-height 10)
  (setq ivy-use-virtual-buffers t)
  ;; Shows x/y where x is current index, y is total
  (setq ivy-count-format "%d/%d ")
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
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<C-return>") 'ivy-immediate-done)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

  (defun tk-cgg-at-point ()
    "Perform a git grep for the symbol at point."
    (interactive)
    (counsel-git-grep
     nil
     (thing-at-point 'sexp)))

  (defun tk-cgg-suffix-at-point ()
    "Perform a git grep for the name at point.
Assumes clojure symbol in the form <namespace>/<name>, and will
just git grep the name.

When I'm not using CIDER, I find myself doing git grep with symbol names
in the context of a project, and this makes it so that typing the name of
a symbol isn't necessary."
    (interactive)
    (let* ((ns-and-name (thing-at-point 'symbol t))
           (name (car (last (split-string ns-and-name "/")))))
      (counsel-git-grep
       nil
       name)))

  (defun tk-swiper-suffix-at-point ()
    "Perform swiper search for the name at point.
Assumes clojure symbol in the form <namespace>/<name>, and will
just git grep the name."
    (interactive)
    (let* ((ns-and-name (thing-at-point 'symbol t))
           (name (car (last (split-string ns-and-name "/")))))
      (swiper
       name)))

  (global-set-key (kbd "M-=") 'tk-cgg-at-point)
  (global-set-key (kbd "C-c .") 'tk-cgg-at-point)
  (global-set-key (kbd "C-c C-.") 'tk-cgg-suffix-at-point)
  (global-set-key (kbd "C-c C-j") 'tk-cgg-suffix-at-point)
  (global-set-key (kbd "C-c C-s") 'tk-swiper-suffix-at-point))

(use-package projectile
  :disabled)

;; search and isolate chars/words in window with a character-based decision tree
(use-package avy
  :ensure t
  ;; avy-goto-char maps every occurence of a typed char to a series of keys.
  ;; avy-goto-word maps every occurence of a typed word that starts with the key typed.
  ;; Typing the matching series will nav to a specific area in the visible buffer.
  ;; In a clojure file, `M-s (` is useful to get to the start of a visible form
  :bind ("C-q" . avy-goto-word-1))

;; Theme Packages
;; ==============
;; highlights the current line
(use-package linum
  :ensure t
  :config
  (setq linum-format "%d")
  (global-linum-mode t))

;; highlights the line number of the current line
(use-package hlinum
  :ensure t
  :config
  (hlinum-activate)
  ;(set-face-background 'linum-highlight-face "#ffb86c")
  ;(set-face-foreground 'linum-highlight-face "#000")
)

;; highlights matching parens
(use-package paren
  :ensure t
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#ffb86c")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;; mode-line customization
(use-package powerline
  :ensure t)

;; mode-line color theme
(use-package moe-theme
  :ensure t
  :config
  (require 'powerline)
  ;(moe-dark) ;; I don't like how this affects switcher
  ;(moe-theme-set-color 'cyan)
  ;(set-face-attribute 'powerline-active2 nil :background "#4e4e4e" :foreground "#3a3a3a")
  ;(set-face-attribute 'powerline-inactive2 nil :background "#4e4e4e" :foreground "#4e4e4e")
  (powerline-moe-theme))

;; main editor color theme
(use-package monokai-theme
  :ensure t
  :config
  ;;(load-theme 'monokai t)
  (setq monokai-height-minus-1 0.8
        monokai-height-plus-1 1.1
        monokai-height-plus-2 1.15
        monokai-height-plus-3 1.2
        monokai-height-plus-4 1.5))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; ability to figure out the face at cursor
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; display css colors inline
(use-package rainbow-mode
  :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package speedbar
  :ensure t)

;; familiar tree-based file navigation
(use-package sr-speedbar
  :ensure t
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-use-images nil)
  (setq sr-speedbar-right-side nil)
  (global-set-key (kbd "C-c n") 'sr-speedbar-toggle))

;; cursor position animation to make it clear where it is
(use-package beacon
  :diminish beacon-mode
  :ensure t
  :config
  (beacon-mode 1)
  ;(setq beacon-color "#FF0000")
  )

(defun personal-filename->icon (filename)
  "Return an icon based on a file or dir FILENAME.  Defaults to `all-the-icons-icon-for-file` after checking for a couple known cases that aren't quite what I want."
  (cond ((string-match ".emacs$" filename) (all-the-icons-fileicon "emacs" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-purple))
        ((string-match ".git/" filename) (all-the-icons-alltheicon "git" :height 1.0 :face 'all-the-icons-lred))
        ((string-match "/$" filename) (all-the-icons-faicon "folder" :height .85 :face 'all-the-icons-blue))
        (t (all-the-icons-icon-for-file filename))))

(defun personal-format-with-icon (icon s)
  "Return a candidate string with ICON followed by S."
  (format "%s %s"
          (propertize " " 'display icon)
          s))

;; Due to font-cache problems, I am using this modification to ensure
;; counsel-find-file behaves the way I want.
(defun personal-all-the-icons-ivy-transformer (filename)
  "Create the \"find-file\" entry using FILENAME to choose an icon."
  (let ((icon (personal-filename->icon filename)))
    (personal-format-with-icon icon filename)))

;; use icons for file types
(use-package all-the-icons
  :ensure t
  ;; Ran M-x all-the-icons-install-fonts to install font cache locally
  )

;; use icons for counsel-find-file
(use-package all-the-icons-ivy
  :ensure t
  :config
  ;; The formatter uses a tab, which by default doesn't look good...
  (setq-default tab-width 2)
  (all-the-icons-ivy-setup)
  (ivy-set-display-transformer 'counsel-find-file 'personal-all-the-icons-ivy-transformer))

;; use icons for dired buffer
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; ensure the mode-line moe-theme
(moe-theme-set-color 'cyan)

;; Editor Packages
;; ===============

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  ;; Useful for completions, used to be what I used for M-x
  ;; C-s to move right, C-r to move left in list
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

;; on delete, remove all whitespace
(use-package hungry-delete
  :diminish hungry-delete-mode
  :ensure t
  :config
  (global-hungry-delete-mode 1))

;; allow ability to expand current text selection
(use-package expand-region
  :ensure t
  :config
  ;; This finally provides a nice way to select and entire s-expr!
  ;; If you are at the starting paren, C-= will select up to the
  ;; matching closing paren. This makes quick C-w for yanking an
  ;; s-expr to move it, tasty!
  (global-set-key (kbd "C-=") 'er/expand-region))

;; automatic re-indentation when code organization changes
(use-package aggressive-indent
  :ensure t
  :config
  ;; Uncomment this to use everywhere.
  ;; I choose not to because not all code I interact with is consistently formatted.
  ;;(global-aggressive-indent-mode t)
  )

;; Clojure Packages
;; ================

;; efficient lisp editing
(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

;; match nested parens with matching colors
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (with-eval-after-load 'cider
    (define-key cider-mode-map (kbd "C-c d") #'cider-doc)))

;; TODO: lookup company mode, seems to be that is the current supported auto-complete
;; (use-package ac-cider
;;   :ensure t
;;   :config
;;   (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;;   (add-hook 'cider-mode-hook 'ac-cider-setup)
;;   (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;;   (eval-after-load "auto-complete"
;;     '(progn
;;        (add-to-list 'ac-modes 'cider-mode)
;;        (add-to-list 'ac-modes 'cider-repl-mode))))

(use-package clojure-snippets
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand))

;; Experimental
;; ============

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

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; a function to quickly insert the current timestamp
(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %X")))

;; insert timestamp into the current buffer
(global-set-key (kbd "C-c i t") 'insert-timestamp)

;; use M-. and M-, for nav to elisp definitions
(use-package elisp-slime-nav
  :ensure t)

;; use C-. and C-, for nav to clojure definitions (without cider)
;; dumb-jump is intentionally lightweight and imprecise, which is
;; usually fine for browsing code in a single repo.
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (global-set-key (kbd "C-.") 'dumb-jump-go)
  (global-set-key (kbd "C-,") 'dumb-jump-back))

;; try out new packages without having to install them
(use-package try
  :ensure t)

(use-package gist
  :ensure t
  :defer t)

(use-package counsel-osx-app
  :ensure t)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "blue violet")
 '(company-quickhelp-color-foreground "thistle1")
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" default)))
 '(package-selected-packages
   (quote
    (counsel-osx-app ob-clojure gist web-mode dumb-jump elisp-slime-nav ag flycheck-joker markdown-mode clojure-snippets flycheck rainbow-mode magit cider clojure-mode rainbow-delimiters paredit sr-speedbar smex monokai-theme moe-theme powerline auto-complete counsel ivy-hydra ace-window org-bullets which-key try use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))
