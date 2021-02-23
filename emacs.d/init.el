;;; -*- lexical-binding: t -*-

(setq user-full-name "Andrew Thompson"
      user-mail-address "github@downthewire.co.uk")

(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-linux* (eq system-type 'gnu/linux))
(setq *is-windows* (eq system-type 'windows-nt))

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-cache-autoloads t)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)
(use-package diminish) ;; for :diminish
(use-package bind-key) ;; for :bind

(use-package use-package-ensure-system-package)

(use-package gcmh
  :hook (after-init . gcmh-mode))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package emacs
  :straight nil
  :init
  ;; answer with y/n instead of typing out yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)
  :custom
  ;; load new source files instead of stale elisp bytecode
  (load-prefer-newer t)
  ;; allow emacs to be any size, removes black bars
  (frame-resize-pixelwise t))

(use-package autorevert
  :straight nil
  :custom
  (global-revert-check-vc-info t)
  :config
  (global-auto-revert-mode +1))

(use-package mule
  :straight nil
  :config
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8-unix)
  (setq locale-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix))

(use-package files
  :straight nil
  :config
  (setq
   backup-by-copying t
   backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-make-backup-files t))

(use-package simple
  :straight nil
  :custom
  ;; killing and yanking uses the system clipboard
  (save-interprogram-paste-before-kill t)
  :config
  ;; display column info in the modeline
  (column-number-mode +1))

(use-package so-long
  :straight nil
  :config
  (global-so-long-mode +1))

(use-package saveplace
  :straight nil
  :config
  (save-place-mode +1))

(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "etc/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(use-package paren
  :straight nil
  :custom
  (show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package general
    :custom
    (general-override-states '(insert emacs hybrid normal visual motion operator replace))
    :config
    (global-set-key (kbd "C-w") 'backward-kill-word)
    (global-set-key (kbd "C-c C-k") 'kill-region)
    (global-set-key (kbd "C-x C-k") 'kill-region)
    (global-set-key "\C-x\C-m" 'helm-M-x)

    (general-override-mode)
    (general-create-definer my-leader-def
      :prefix "C-c")
    (my-leader-def
      "c" 'comment-dwim
      "RET" 'make-frame-command
      ;; bookmarks
     "r" '(:ignore t :wk "bookmarks")
      "rm" 'bookmark-set
      "rb" 'bookmark-jump
      "rl" 'bookmark-bmenu-list
      ;; quit / restart
      "q" '(:ignore t :wk "quit / restart")
      "qq" 'save-buffers-kill-terminal
      "qr" 'restart-emacs)
)

(use-package which-key
  :custom
  (which-key-idle-delay 0)
  :config
  (which-key-mode +1)
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom))

(use-package hydra
  :defer t
  :general
  (my-leader-def
   "P" '(hydra-straight-helper/body :wk "pkgs"))
  :config
  (defhydra hydra-zoom (global-map "<f5>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")
    ("r" (text-scale-set 0) "reset")
    ("0" (text-scale-set 0) :bind nil :exit t))
  (defhydra hydra-straight-helper (:hint nil :color green)

    "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
    ("c" straight-check-all)
    ("C" straight-check-package)
    ("r" straight-rebuild-all)
    ("R" straight-rebuild-package)
    ("f" straight-fetch-all)
    ("F" straight-fetch-package)
    ("p" straight-pull-all)
    ("P" straight-pull-package)
    ("m" straight-merge-all)
    ("M" straight-merge-package)
    ("n" straight-normalize-all)
    ("N" straight-normalize-package)
    ("u" straight-push-all)
    ("U" straight-push-package)
    ("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build)
    ("q" nil)))

(use-package keyfreq
  :config
  (keyfreq-autosave-mode 1))

(use-package helm
   :diminish
   :init (helm-mode t)
   :bind (("M-x"     . helm-M-x)
          ("C-x C-f" . helm-find-files)
          ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
          ("C-x r b" . helm-filtered-bookmarks)
          ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
          ("C-c i"   . helm-imenu)
          ("C-h a"   . helm-apropos)
          ;; Look at what was cut recently & paste it in.
          ("M-y" . helm-show-kill-ring)

          :map helm-map
          ;; We can list ‘actions’ on the currently selected item by C-z.
          ("C-z" . helm-select-action)
          ;; Let's keep tab-completetion anyhow.
          ("TAB"   . helm-execute-persistent-action)
          ("<tab>" . helm-execute-persistent-action)))

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-bookmark-set
                                      helm-source-buffer-not-found))

(use-package helm-descbinds
  :config (helm-descbinds-mode))

(use-package helm-make)

(use-package helm-swoop
  :config (setq helm-swoop-pre-input-function
		(lambda () ""))
  :bind  (("C-s"     . 'helm-swoop)
          ("C-M-s"   . 'helm-multi-swoop-all)
          ("C-S-s" . 'helm-swoop-back-to-last-point))
  :custom (helm-swoop-split-with-multiple-windows nil "Do not split window inside the current window."))

(use-package anzu
  :config
  (global-anzu-mode)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :config
    ;; Always have it on
    (global-undo-tree-mode)
    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)
    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

(use-package avy
  :bind ("C-'" . avy-goto-char-timer))

(use-package multiple-cursors
  :defer t
  :general
  (my-leader-def
    "v" 'mc/edit-lines))

(use-package iedit)

(use-package dashboard
  :straight nil
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-init-info (concat "Welcome "     user-full-name
                                    "! Emacs "      emacs-version
                                    "; System "     (system-name)
                                    "; Time "       (emacs-init-time))))

(use-package esup
  :commands (esup))

(use-package emacs
  :straight nil
  :when *is-a-mac*
  :config
  (setq mac-command-modifier 'meta) ;; Mac atl/option to Control
  (setq mac-option-modifier 'control) ; Mac command to Meta
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(use-package cus-edit
  :straight nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (if (file-exists-p custom-file)
      (load-file custom-file)))

(use-package frame
  :straight nil
  :config
  (blink-cursor-mode -1)
  (setq initial-scratch-message ""
        inhibit-startup-message t
        visible-bell nil
        ring-bell-function 'ignore
        initial-frame-alist
        '((menu-bar-lines . 0)
          (tool-bar-lines . 0)))
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(use-package dracula-theme
  :config
  (load-theme 'dracula))

(add-to-list 'default-frame-alist '(font . "Fira Code-14"))

(use-package all-the-icons
  :defer t)

(use-package doom-modeline
  :demand t
  :preface
  (defun my-doom-modeline-setup ()
    (column-number-mode +1)
    (doom-modeline-mode +1))
  :init (my-doom-modeline-setup)
  :custom
  (doom-modeline-vcs-max-length 50)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-delay  0)
  (setq show-paren-style 'mixed))

(use-package beacon
  :diminish
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

(use-package golden-ratio
  :diminish 
  :init (golden-ratio-mode 1))

(use-package ace-window
  :straight nil
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(setq ibuffer-saved-filter-groups
      '(("home"
         ("system-config" (or (filename . "dotfiles")
                              (filename . "emacs-config")))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("code" (or (filename . "code")
                     (filename . ".py")
                     (filename . ".go")
                     (filename . ".java")))
         ("Web Dev" (or (mode . html-mode)
                        (mode . css-mode)))
         ("Directories" (mode . dired-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("Builtin" (or (name . "\*Messages\*")
                        (name . "\*Completions\*")
                        (name . "\*Backtrace\*")
                        (name . "\*Compile-Log\*")
                        (name . "\*Calendar\*")
                        (name . "\*Calculator\*")
                        (name . "'*Scratch\*"))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))
; hide empty filter groups
(setq ibuffer-show-empty-filter-groups nil)

(use-package dired
  :straight nil
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :general
  (my-leader-def
    "d" 'dired))

;; Colourful columns.
(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode +1))

(use-package dired-git-info
    :bind (:map dired-mode-map
                ("C-(" . dired-git-info-mode)))

(use-package projectile
  :config
  (projectile-mode +1))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

(use-package treemacs
  :defer t
  :general ([f8] 'treemacs))

(use-package treemacs-projectile
  :after (projectile treemacs))

(use-package treemacs-magit
  :after (treemacs))

(defun my-org-prettify-hook ()
  (turn-on-visual-line-mode))

(use-package org-superstar
  :ensure t
  :defer 3
  :hook (org-mode . org-superstar-mode)
  :init(setq org-superstar-prettify-item-bullets t
             org-superstar-leading-bullet ?\s
             org-hide-leading-stars t
             org-superstar-item-bullet-alist '((?* . ?●)
                                               (?+ . ?➤)
                                               (?- . ?—))))

(defun my-org-prettify-settings ()
  (setq org-startup-indented t
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        line-spacing 0.2))

(use-package htmlize
  :defer t)

(defun my-org-todo-setup ()
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "CURRENT(c)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(a@/!)")))
  (setq org-log-done 'time))

(defun my-org-structure-templates ()
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org-capture
  :straight nil
  :general
  (my-leader-def
    "C" 'org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/agenda/todo.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("s" "Standup" entry (file+olp+datetree "~/org/agenda/todo.org" "Standup")
           "* Planned\n- %?\n %i\n %a")))

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("b"
                 "Blog Post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "blog-posts.org" "Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))

(use-package org
  :straight nil
  :gfhook
  #'my-org-prettify-hook
  ('org-src-mode-hook #'my-disable-flycheck-for-elisp)
  :preface
  (defun my-disable-flycheck-for-elisp ()
    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :custom
  (if *is-windows*
      (setq org-agenda-files (list "C:\\Dropbox\\Andrew\\org\\"))
    (setq org-agenda-files (list "~/Dropbox/Andrew/org" "~/Dropbox/Andrew/org/journal")))

  :config
  (if *is-windows*
      (setq org-directory "C:\\Dropbox\\Andrew\\org\\")
    (setq org-directory "~/Dropbox/Andrew/org/"))
  (define-key org-mode-map (kbd "C-'") nil) ;; This conflicts with avy
  (my-org-prettify-settings)
  (my-org-todo-setup)
  (my-org-structure-templates))

(use-package org-pomodoro)

(use-package org-journal
  :straight nil
  :defer t
  :config (if *is-windows*
            (setq org-journal-dir "C:\\Dropbox\\Andrew\\org\\journal")
          (setq org-journal-dir "~/Dropbox/Andrew/org/journal/"))
  (setq org-journal-date-format "%A %d %B %Y")
  (setq org-journal-time-format "%H:%M")
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-file-format "%Y%m%d.org")
  :bind ("C-x C-j" . org-journal-new-entry))

(use-package org-babel
  :no-require
  :straight nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(if *is-windows*
    (add-to-list 'exec-path "C:\\bin"))

(use-package org-roam
  :config (if *is-windows*
              (setq org-roam-directory "C:\\Dropbox\\Andrew\\org\\roam")
            (setq org-roam-directory "~/Dropbox/Andrew/org/roam"))
  :hook (add-hook 'after-init-hook 'org-roam-mode))

(defhydra hydra-org-roam (:exit t :idle 0.8)
  "Launcher for `org-roam'."
  ("i" org-roam-insert "insert")
  ("f" org-roam-find-file "find-file")
  ("v" org-roam-buffer-activate "backlinks"))
(global-set-key (kbd "C-c r") 'hydra-org-roam/body)

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  (org-mode . anki-editor-mode)
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t
        anki-editor-use-math-jax t
        org-my-anki-file org-default-notes-file)
        ;; Org-capture templates
  (add-to-list 'org-capture-templates
               '("a" "Anki basic"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
               '("A" "Anki cloze"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))
  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

(use-package olivetti
  :defer t
  :custom
  (olivetti-body-width 90))

(use-package writegood-mode
  :defer t)

(defun my/writing-modes ()
  (interactive)
  (flyspell-mode +1)
  (olivetti-mode +1)
  (writegood-mode +1))

(setenv "LANG" "en_GB")
(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
       ((org-mode text-mode) . flyspell-mode))
  :config
  (if *is-windows*
      (progn
      (setq ispell-program-name "C:\\ProgramData\\chocolatey\\bin\\hunspell.exe")
      (add-to-list 'ispell-dictionary-alist '("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8))
      (setq ispell-local-dictionary-alist ispell-dictionary-alist)
      (setq ispell-hunspell-dictionary-alist ispell-dictionary-alist))
    (setq ispell-program-name "/usr/local/bin/aspell"))
  (setq ispell-dictionary "en_GB")
  (setq ispell-silently-savep t)
  (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws"))

(use-package display-line-numbers
  :straight nil
  :ghook
  ('prog-mode-hook #'display-line-numbers-mode))

(use-package flycheck
  :config
  (global-flycheck-mode +1))

(use-package company
  :diminish
  :config
  :ensure t
  :config
  (global-company-mode 1)
  (setq ;; Only 1 letters required for completion to activate.
   company-minimum-prefix-length 1
   ;; Search other buffers for compleition candidates
   company-dabbrev-other-buffers t
   company-dabbrev-code-other-buffers t
   ;; Show candidates according to importance, then case, then in-buffer frequency
   company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence)
   ;; Flushright any annotations for a compleition;
   ;; e.g., the description of what a snippet template word expands into.
   company-tooltip-align-annotations t
   ;; Allow (lengthy) numbers to be eligible for completion.
   company-complete-number t
   ;; M-⟪num⟫ to select an option according to its number.
   company-show-numbers t
   ;; Show 10 items in a tooltip; scrollbar otherwise or C-s ^_^
   company-tooltip-limit 10
   ;; Edge of the completion list cycles around.
   company-selection-wrap-around t
   ;; Do not downcase completions by default.
   company-dabbrev-downcase nil
   ;; Even if I write something with the ‘wrong’ case,
   ;; provide the ‘correct’ casing.
   company-dabbrev-ignore-case nil
   ;; Immediately activate completion.
   company-idle-delay 0)

  ;; Use C-/ to manually start company mode at point. C-/ is used by undo-tree.
  ;; Override all minor modes that use C-/; bind-key* is discussed below.
  (bind-key* "C-/" #'company-manual-begin)

  ;; Bindings when the company list is active.
  :bind (:map company-active-map
              ("C-d" . company-show-doc-buffer) ;; In new temp buffer
              ("<tab>" . company-complete-selection)
              ;; Use C-n,p for navigation in addition to M-n,p
              ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
              ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))))

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
    (go-mode . lsp-deferred)
  :commands lsp
  :custom
  (lsp-completion-provider :capf)
  (lsp-keymap-prefix "C-l"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-enable t
                lsp-ui-peek-enable t
                lsp-ui-sideline-enable t
                lsp-ui-imenu-enable t
                lsp-ui-flycheck-enable t))

(use-package helm-lsp :commands (helm-lsp-workspace-symbol))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package yasnippet
 :custom
 (yas-snippet-dirs
  '("~/.emacs.d/snippets"))
 :config
 (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode +1))

(use-package magit
  :defer t
  :general
  ("C-x g" 'magit-status)
  (my-leader-def
    "g" '(:ignore t :wk "git")
    "gs" 'magit-status
    "gc" 'magit-checkout
    "gC" 'magit-commit
    "gb" 'magit-blame
    "gS" 'magit-stage-file
    "gU" 'magit-unstage-file
    "gg" 'hydra-my-git-menu/body
    "gy" 'my/magit-yank-branch-name)
  :config
  (defun my/magit-yank-branch-name ()
    "Show the current branch in the echo-area and add it to the `kill-ring'."
    (interactive)
    (let ((branch (magit-get-current-branch)))
      (if branch
          (progn (kill-new branch)
                 (message "%s" branch))
        (user-error "There is not current branch")))))

(use-package git-timemachine
  :defer t)

(use-package git-messenger
  :defer t)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1)
  (setq-default fringes-outside-margins t))

(use-package git-link
  :general
  (my-leader-def
    "gl" '(:ignore t :wk "git link")
    "gll" 'git-link
    "glc" 'git-link-commit
    "glh" 'git-link-homepage))

(use-package browse-at-remote
  :general
  (my-leader-def
    "glg" 'browse-at-remote))

(defhydra hydra-my-git-menu (global-map "<f7>"
                                        :color blue)
  "
^Navigate^        ^Action^               ^Info^
^^^^^^^^^^^^---------------------------------------------------
_j_: next hunk    _s_: stage hunk        _d_: diff
_k_: prev hunk    _S_: stage file        _c_: show commit
^ ^               _U_: unstage file      _g_: magit status
^ ^               ^ ^                    _t_: git timemachine
^ ^               ^ ^                    ^ ^
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("s" git-gutter:stage-hunk)
  ("S" magit-stage-file)
  ("U" magit-unstage-file)
  ("c" git-messenger:popup-show)
  ("g" magit-status :exit t)
  ("d" magit-diff-buffer-file)
  ("t" git-timemachine :exit t)
  ("q" quit-window "quit-window")
  ("<ESC>" git-gutter:update-all-windows "quit" :exit t))

(defhydra hydra-my-git-timemachine-menu (:color blue)
  ("s" git-timemachine "start")
  ("j" git-timemachine-show-next-revision "next revision")
  ("k" git-timemachine-show-previous-revision "prev revision")
  ("c" git-timemachine-show-current-revision "curr revision")
  ("<ESC>" git-timemachine-show-current-revision "quit" :exit t))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

;; (when (eq system-type 'darwin)
;;   (mac-auto-operator-composition-mode))

(when (eq system-type 'darwin)
  (setq python-shell-interpreter "/usr/local/bin/python3"))

(when (eq system-type 'darwin)
  (setq visible-bell nil
        ring-bell-function 'flash-mode-line)
  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line)))

(when (eq system-type 'darwin)
  (setq magit-git-executable "/usr/bin/git"))

(use-package restart-emacs
  :defer t)

(use-package restclient
  :defer  t)

(use-package company-restclient
  :defer t)

(use-package ob-restclient
  :defer t)

(use-package go-mode
    :custom
    (defun lsp-go-install-save-hooks ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t))
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  :general
  (my-leader-def
   "p" '(hydra-go-mode/body :wk "go-mode"))
  :config
  (defhydra hydra-go-mode (:hint nil :color green)

    "
Imports             Describe             GoTo
--------------------------------------------------------
_ig_ import go      _d_escribe           _ga_ arguments
_ia_ import add     _j_ump to definition _gd_ docstring
_ir_ import remove  ^ ^                  _gf_ function
^ ^                 ^ ^                  _gn_ function name
^ ^                 ^ ^                  _gr_ return values
"
    ("ig" go-goto-imports)
    ("ia" go-import-add)
    ("ir" go-remove-unused-imports)
    ("d" godef-describe)
    ("j" godef-jump)
    ("ga" go-goto-arguments)
    ("gd" go-goto-docstring)
    ("gf" go-goto-function)
    ("gn" go-goto-function-name)
    ("gr" go-goto-return-values)))

(use-package dockerfile-mode
  :defer t)

(use-package docker
  :defer t)

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . turn-on-haskell-indent))

(use-package js2-mode
  :mode "\\.js$"
  :hook (js2-mode . lsp)
  :interpreter "node"
  :ensure-system-package ((typescript-language-server . "npm i -g typescript-language-server")
                          (eslint_d . "npm i -g eslint_d"))
  :custom
  ;; set the indent level to 2
  (js2-basic-offset 2)
  (js-chain-indent t)
  (js-indent-level 2)
  ;; use eslint_d instead of eslint for faster linting
  (flycheck-javascript-eslint-executable "eslint_d"))

(use-package json-mode
  :mode "\\.json\\'")

(use-package skewer-mode
  :defer t
  :ghook ('js2-mode-hook)
  :general
  (my-local-leader-def 'js2-mode-map
    "eb" 'skewer-eval-defun
    "el" 'skewer-eval-last-expression))

(which-key-add-major-mode-key-based-replacements 'clojure-mode "C-c e" "eval")
(which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode "C-c e" "eval")
(which-key-add-major-mode-key-based-replacements 'hy-mode "C-c e" "eval")
(which-key-add-major-mode-key-based-replacements 'lisp-interaction-mode "C-c e" "eval")
(which-key-add-major-mode-key-based-replacements 'scheme-mode "C-c e" "eval")

(defconst my-lisp-mode-hooks
  '(lisp-mode-hook
    sly-mrepl-mode-hook
    emacs-lisp-mode-hook
    scheme-mode-hook
    geiser-repl-mode-hook
    hy-mode-hook
    inferior-hy-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook))

(defun my-lisp-setup ()
  (electric-pair-mode -1))

(use-package paredit
  :defer t
  :ghook my-lisp-mode-hooks
  :gfhook #'my-lisp-setup)

(my-leader-def
  :keymaps 'emacs-lisp-mode-map
  "eb" 'eval-buffer
  "el" 'eval-last-sexp
  "ed" 'eval-defun
  "er" 'eval-region)

(my-leader-def
  :keymaps 'lisp-interaction-mode-map
  "eb" 'eval-buffer
  "el" 'eval-last-sexp
  "ed" 'eval-defun
  "er" 'eval-region)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(use-package clojure-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-enable-indentation nil))

(use-package cider
  :after clojure-mode
  :hook (cider-repl-mode . rainbow-delimiters-mode)
  :general
  (my-local-leader-def 'clojure-mode-map
    "r" 'cider
    "n" 'cider-repl-set-ns
    "er" 'cider-eval-region
    "eb" 'cider-eval-buffer
    "el" 'cider-eval-last-sexp))

(use-package sly
  :defer t
  :hook (sly-mrepl-mode . rainbow-delimiters-mode)
  :general
  (my-local-leader-def
    :keymaps 'lisp-mode-map
    "eb" 'sly-eval-buffer
    "el" 'sly-eval-last-expression
    "ed" 'sly-eval-defun
    "er" 'sly-eval-region)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package sly-quicklisp
  :after sly)

(use-package sly-asdf
  :after sly)

(use-package hy-mode
  :mode "\\.hy\\'"
  :general
  (my-local-leader-def 'hy-mode-map
    "er" 'hy-shell-eval-region
    "eb" 'hy-shell-eval-buffer
    "el" 'hy-shell-eval-last-sexp
    "ed" 'hy-shell-eval-current-form))

(use-package geiser
  :defer t
  :general
  (my-local-leader-def
    :keymaps 'scheme-mode-map
    "r" 'run-geiser
    "er" 'geiser-eval-region
    "eR" 'geiser-eval-region-and-go
    "eb" 'geiser-eval-buffer
    "eB" 'geiser-eval-buffer-and-go
    "ed" 'geiser-eval-definition
    "eD" 'geiser-eval-definition-and-go
    "el" 'geiser-eval-eval-sexp)
  :custom
  (geiser-active-implementations '(guile mit racket)))

(use-package python
  :mode "\\.py\\'"
  :ghook
  ('python-mode-hook #'lsp)
  :general
  (my-local-leader-def 'python-mode-map
    "er" 'python-shell-send-region
    "eb" 'python-shell-send-buffer
    "ef" 'python-shell-send-file
    "es" 'python-shell-send-string))

(use-package pipenv
  :hook ((python-mode . pipenv-mode)
         (hy-mode . pipenv-mode))
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package web-mode
  :defer t
  :preface
  (defun my-web-mode-hook ()
    ;; set the html indent to 2
    (setq web-mode-markup-indent-offset 2)
    ;; highlight matching elements in html
    (setq web-mode-enable-current-element-highlight 1))
  :hook (web-mode . my-web-mode-hook)
  :init
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos))))))
  (add-to-list `auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list `auto-mode-alist '("\\.css\\'" . web-mode)))

(use-package yaml-mode
  :defer t)
