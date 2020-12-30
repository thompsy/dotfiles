(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-linux* (eq system-type 'gnu/linux))
(setq *is-windows* (eq system-type 'windows-nt))


(setq debug-on-error t)
;(setq url-user-agent "curl/7.29.0")
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Package setup
;;
(require 'package)
; list the packages to auto-install
(setq package-list '(ace-jump-mode ace-window avy beacon color-theme-sanityinc-tomorrow company dashboard diminish dimmer docker-compose-mode dockerfile-mode helm-descbinds helm-projectile helm-swoop helm helm-core highlight-parentheses jinja2-mode magit git-commit markdown-toc markdown-mode monokai-theme page-break-lines pdf-tools popup projectile-sift projectile pkg-info epl rainbow-delimiters restart-emacs sift smartscan spaceline-all-the-icons spaceline s dash powerline all-the-icons memoize spacemacs-theme synosaurus tablist transient use-package bind-key which-key with-editor async yaml-mode expand-region))

(setq package-archives '(
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
; fetch the list of packages available and install any missing
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic UI Tweaks
;;
(blink-cursor-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

; You can also set the initial frame parameters
(setq initial-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)))

; display the column number too
(column-number-mode 1)
; period, single space ends sentences
(setq sentence-end-double-space nil)
; just y or n rather than yes or no
(fset 'yes-or-no-p 'y-or-n-p)
; make it easy to find the cursor
(global-hl-line-mode t)
; disable the audible bell
(setq visible-bell 1)
; treat CamelCase as separate words
(global-subword-mode 1)
(diminish  'subword-mode)

(setq frame-title-format '("" "%b"))

(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Themes
;;
(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow--define-theme eighties))
;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula :defer t)
(load-theme 'dracula)
;(use-package spacemacs-common
;  :defer t
;  :ensure spacemacs-theme)

(setq display-time-day-and-date t)
(display-time)
(use-package fancy-battery
  :defer t
  :diminish
  :custom (fancy-battery-show-percentage  t)
          (battery-update-interval       15)
	  :config (fancy-battery-mode))

(line-number-mode t)
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode      t)


(use-package spaceline
  :custom (spaceline-buffer-encoding-abbrev-p nil)
          (spaceline-line-column-p t) ;; Show “line-number : column-number” in modeline.
          (powerline-default-separator 'arrow)
  :config (require 'spaceline-config)
          (spaceline-helm-mode)
          (spaceline-info-mode)
          (spaceline-emacs-theme))
(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(setq-default
 powerline-height 24
 powerline-default-separator 'wave
 spaceline-flycheck-bullet "❖ %s"
 spaceline-separator-dir-left '(right . right)
 spaceline-separator-dir-right '(left . left))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mac specific key settings
;;
(when *is-a-mac*
  (setq mac-command-modifier 'control) ;; Mac atl/option -> Control
  (setq mac-option-modifier 'meta) ; Mac command -> Meta
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-14")))
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(setq default-frame-alist 
     (add-to-list 'default-frame-alist '(font . "Fira Code-14")))

;(when (featurep 'ns)
;  (defun ns-raise-emacs ()
;    "Raise Emacs."
;    (ns-do-applescript "tell application \"Emacs\" to activate"))(defun ns-raise-emacs-with-frame (frame)
;    "Raise Emacs and select the provided frame."
;    (with-selected-frame frame
;      (when (display-graphic-p)
;        (ns-raise-emacs))))(add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)(when (display-graphic-p)
;    (ns-raise-emacs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General Keybindings
;;
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Backups
;;
(setq
 backup-by-copying t
 backup-directory-alist
 '((".*" . "~/.emacs.d/backups/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which key
;; 
;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :defer 5
  :config (which-key-mode)
	  (which-key-setup-side-window-bottom)
	  (setq which-key-idle-delay 0.05))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode
;;
(if *is-windows*
    (setq org-agenda-files (list "C:\\Dropbox\\Andrew\\org\\"))
  (setq org-agenda-files (list "~/workspace/scratch/" "~/Dropbox/Andrew/org")))

(use-package org-mode
  :hook (add-hook 'org-mode-hook #'visual-line-mode))

(use-package org-bullets
  :if *is-a-mac*
  :hook (org-mode . org-bullets-mode))

(use-package org-journal
  :ensure t
  :defer t
  :config (if *is-windows*
	      (setq org-journal-dir "C:\\Dropbox\\Andrew\\org\\journal")
	    (setq org-journal-dir "~/workspace/scratch/journal/"))
  (setq org-journal-date-format "%A %d %B %Y")
  (setq org-journal-time-format "%H:%M")
  :bind ("C-c C-j" . org-journal-new-entry))


;; Set path to sqlite3 for org-roam
(if *is-windows*
    (add-to-list 'exec-path "C:\\bin"))

(use-package org-roam
  :config (if *is-windows*
	      (setq org-roam-directory "C:\\Dropbox\\Andrew\\org\\roam")
	    (setq org-roam-directory "~/Dropbox/Andrew/org/roam"))
  :hook (add-hook 'after-init-hook 'org-roam-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; beacon to easily find the cursor
;;
(use-package beacon
  :diminish
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keep window ratios sweet
;;
(use-package golden-ratio
  :disabled
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; iBuffer
;;
;; Use human readable Size column instead of original one
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allow tree-semantics for undo operations.
;;
(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :config
    ;; Always have it on
    (global-undo-tree-mode)
    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)
    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

(global-set-key (kbd "C-'") 'avy-goto-word-or-subword-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm mode
;;
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

;; Taken from: https://github.com/alhassy/emacs.d#being-at-the-helm----completion--narrowing-framework
; but not working at the moment...
(use-package helm-swoop
  :bind  (("C-s"     . 'helm-swoop)
          ("C-M-s"   . 'helm-multi-swoop-all)
          ("C-S-s" . 'helm-swoop-back-to-last-point))
  :custom (helm-swoop-speed-or-color nil "Give up colour for speed.")
          (helm-swoop-split-with-multiple-windows nil "Do not split window inside the current window."))

;; Helm - Describe Keybindings - C-h b
(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile mode for project management
;;
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-projectile
  :config
  (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; magit
;; 
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; perspective
;;
(use-package perspective
  :defer t
  :config ;; Activate it.
          (persp-mode)
          ;; In the modeline, tell me which workspace I'm in.
          (persp-turn-on-modestring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rainbow delimiters mode
;;
(use-package rainbow-delimiters
;  :disabled
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode)
  :config
  (setq show-paren-delay  0)
  (setq show-paren-style 'mixed)
  (show-paren-mode)
  (electric-pair-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dimmer - dim the frames not being used
;;
(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t)
  (setq dimmer-fraction 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Highlight matching parentheses
;;
(use-package highlight-parentheses
  :config
  (add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add ace window for easier window management
;;
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dashboard
;;
(use-package dashboard
  :ensure t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Provides only the command “restart-emacs”.
;;
(use-package restart-emacs
  :defer t
  :config (defalias 'emacs-restart #'restart-emacs)
  ; Keep open files open across sessions.
  (desktop-save-mode 1)
  (setq desktop-restore-eager 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-completion
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spelling
;;
(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
	 ((org-mode text-mode) . flyspell-mode))
  :config
  (if *is-windows*
      (setq ispell-program-name "C:\\ProgramData\\chocolatey\\bin\\hunspell.exe")
    (setq ispell-program-name "/usr/local/bin/aspell"))
  (setq ispell-dictionary "en_GB")
  (setq  ispell-extra-args '("--sug-mode=ultra"
			     "--run-together"
			     "--run-together-limit=5"
			     "--run-together-min=2"))
  (setq ispell-silently-savep t)
  (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lsp mode for Golang
;;
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package helm-lsp
  :commands (helm-lsp-workspace-symbol))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package dap-mode)
(use-package dap-go)
(use-package go-dlv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Optional - provides snippet support.
;;
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define function to shutdown emacs server instance
;;
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Smart Scan - what does this do?
;;
(use-package smartscan
  :defer t
  :config
    (global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
    (global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)
    (global-set-key (kbd "M-'") 'my/symbol-replace))

(defun my/symbol-replace (replacement)
  "Replace all standalone symbols in the buffer matching the one at point."
  (interactive  (list (read-from-minibuffer "Replacement for thing at point: " nil)))
  (save-excursion
    (let ((symbol (or (thing-at-point 'symbol) (error "No symbol at point!"))))
      (beginning-of-buffer)
      ;; (query-replace-regexp symbol replacement)
      (replace-regexp (format "\\b%s\\b" (regexp-quote symbol)) replacement))))

(setq next-line-add-newlines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Avy
;;
(global-set-key (kbd "C-j") 'avy-goto-char-2)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expand region!
;;
(use-package expand-region
  :config
  (global-set-key (kbd "M-,") 'er/expand-region)
  (global-set-key (kbd "M-.") 'er/contract-region))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(org-bullets org-journal dracula-theme keyfreq counsel counsel-projectile swiper doom-themes helm-lsp go-mode yasnippet lsp-ui lsp-mode which-key use-package undo-tree synosaurus spacemacs-theme spaceline-all-the-icons smartscan restart-emacs rainbow-delimiters projectile-sift pdf-tools monokai-theme monokai-pro-theme monokai-alt-theme markdownfmt markdown-toc markdown-mode+ magit jinja2-mode highlight-parentheses helm-swoop helm-projectile helm-descbinds helm-dash helm-company fancy-battery expand-region dockerfile-mode docker-compose-mode docker dimmer diminish dashboard color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized beacon all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer all-the-icons-gnus all-the-icons-dired ace-window ace-jump-mode))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
