;; Disable package.el
(setq package-enable-at-startup nil)
;; Temporary Theme------------------------------------------------------------------------
(load-theme 'wombat)

;; Basic UI Configuration ----------------------------------------------------------------
;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 125)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
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
                       		 :weight 'light
				 :height 160)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" 
		    		     :weight 'light 
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
  '(use-package :type git :host github :repo "jwiegley/use-package"))
(eval-when-compile 
  (add-to-list 'load-path "~/.emacs.d/straight/build")
  (require 'use-package))
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;; General Dependencies------------------------------------------------------------------
;; Async
(use-package async
	     :straight (:host github :repo "jwiegley/emacs-async"
			:branch "master"))
;; Popup
(use-package popup
	     :straight (:host github :repo "auto-complete/popup-el"
			:branch "master"))

(use-package emojify
	     :straight (:host github :repo "iqbalansari/emacs-emojify"
			:branch "master"))
;; Helm----------------------------------------------------------------------------------
(use-package helm 
	     :straight (:host github :repo "emacs-helm/helm"
			:branch "master"))

;; Evil----------------------------------------------------------------------------------
(use-package evil
	     :straight (:host github :repo "emacs-evil/evil"
			:branch "master"))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
(setq evil-undo-system 'undo-tree)

;; Activate the Evil
(evil-mode 1)

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

;; Org Submodes---------------------------------------------------------------------------
;; Org
(use-package org
	     :straight (:host github :repo "bzg/org-mode"
			:branch "main"))



;; Org Roam
(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
             :files (:defaults "extensions/*"))
  :init (setq org-roam-v2-ack t))

