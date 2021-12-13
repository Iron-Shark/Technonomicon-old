;; Disable package.el
(setq package-enable-at-startup nil)
;; Temporary Theme------------------------------------------------------------------------
(load-theme 'wombat)

;; Basic UI Configuration ----------------------------------------------------------------
;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 125)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        	; Disable visible scrollbar
(tool-bar-mode -1)          	; Disable the toolbar
(tooltip-mode -1)           	; Disable tooltips
(set-fringe-mode 5)        	; Give some breathing room
(menu-bar-mode -1)            	; Disable the menu bar
;; Set up the visible bell
(setq visible-bell t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font Configuration --------------------------------------------------------------------
(set-face-attribute 'default nil :font "JetBrains Mono"
                       		 :weight 'semibold
				 :height 160)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" 
		    		     :weight 'semibold 
				     :height 160)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Overpass" 
					:weight 'regular)
		    			:height 160 

;; Set Location Data----------------------------------------------------------------------
(setq calendar-latitude 42.33)
(setq calendar-longitude -83.04)
(setq calendar-location-name "Detroit,MI")
(column-number-mode)
(global-display-line-numbers-mode t)

;; Package Management---------------------------------------------------------------------
;; Detirmines if straight.el is present.
(unless (featurep 'straight)

;; Bootstrap straight.el
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
  (load bootstrap-file nil 'nomessage)))

(setq straight-repository-branch "develop")

;; Configure use-package
(straight-use-package 
  '(use-package :type git 
		:host github 
		:repo "jwiegley/use-package"))

(eval-when-compile 
  (add-to-list 'load-path "~/.emacs.d/straight/build")
  (require 'use-package))

(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;; System Dependencies------------------------------------------------------------------
;; Async
(use-package async
	     :straight (:host github 
			:repo "jwiegley/emacs-async"
			:branch "master"))
;; Popup
(use-package popup
	     :straight (:host github 
			:repo "auto-complete/popup-el"
			:branch "master"))
;; Emojify 
(use-package emojify
	     :straight (:host github 
			:repo "iqbalansari/emacs-emojify"
			:branch "master"))
;; Undo Tree
(use-package undo-tree
	     :straight (:host github 
			:repo "emacs-straight/undo-tree"
			:branch "master"))

;; Which-key
(use-package which-key 
	     :straight (:host github
			:repo "justbur/emacs-which-key" 
			:branch "master")
	     	        :defer 0
  			:diminish which-key-mode
  			:config
  			(which-key-mode)
  			(setq which-key-idle-delay 1))

;; Helpful
(use-package helpful 
	     :straight (:host github 
		        :repo "Wilfred/helpful"
		        :branch "master")
               	        :commands (helpful-callable helpful-variable helpful-command helpful-key)
                        :custom
                        (counsel-describe-function-function #'helpful-callable)
                        (counsel-describe-variable-function #'helpful-variable)
                        :bind
                        ([remap describe-command] . helpful-command)
                        ([remap describe-key] . helpful-key))

;; Helm----------------------------------------------------------------------------------
(use-package helm 
	     :straight (:host github 
			:repo "emacs-helm/helm"
			:branch "master"))
	     ;:bind

;; Set Global Helm Keys
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c h") #'helm-command-prefix)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun runemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'runemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)


(helm-mode 1)

;; VI ------------------------------------------------------------------------------------
;; Evil
(use-package evil
	     :straight (:host github 
			:repo "emacs-evil/evil"
			:branch "master")
	     :init
	     (setq evil-want-integration t)
	     (setq evil-want-keybinding nil)
	     (setq evil-want-C-u-scroll t)
	     (setq evil-want-C-i-jump nil)
	     (setq evil-respect-visual-line-mode t)
	     (setq evil-undo-system 'undo-tree)
	     :config
	     (evil-mode 1)
	     (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	     (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

;; Evil Collection
(use-package evil-collection
	     :straight (:host github 
			:repo "emacs-evil/evil-collection"
			:branch "master"))

;; General Keybindings
(use-package general
	     :straight (:host github 
			:repo "noctuid/general.el"
			:branch "master")
	     :after evil
             :config
             (general-create-definer efs/leader-keys
               :keymaps '(normal insert visual emacs)
               :prefix "SPC"
               :global-prefix "C-SPC")

             (efs/leader-keys
               "t"  '(:ignore t :which-key "toggles")))

;; Set Emacs state modes
(dolist (mode '(custom-mode
                eshell-mode
                git-rebase-mode
                erc-mode
                circe-server-mode
                circe-chat-mode
                circe-query-mode
                sauron-mode
                term-mode))
        (add-to-list 'evil-emacs-state-modes mode))

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;; Org Submodes---------------------------------------------------------------------------
;; Org
(use-package org
	     :straight (:host github 
			:repo "bzg/org-mode"
			:branch "main")
  			:commands (org-capture org-agenda)
;  			:hook (org-mode . efs/org-mode-setup)
  			:config
  			(setq org-ellipsis " ▾"))

;; Org Roam
(use-package org-roam
  :straight (:host github 
	     :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :init (setq org-roam-v2-ack t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))
(setq org-roam-directory
      (file-truename "~/Archive/Wiki/Nodes"))

;; Fly-X----------------------------------------------------------------------------------
;; FlySpell
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
	(setq ispell-program-name "~/.guix-profile/bin/hunspell")
	(flyspell-mode 1))))
;;TODO Install Flycheck
