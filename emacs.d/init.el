(setq package-enable-at-startup nil)

;; Basic UI Configuration ----------------------------------------------------------------
;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 150)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        	; Disable visible scrollbar
(tool-bar-mode -1)          	; Disable the toolbar
(tooltip-mode -1)           	; Disable tooltips
(set-fringe-mode 5)        	; Give some breathing room
(menu-bar-mode -1)            	; Disable the menu bar
(column-number-mode)
(global-display-line-numbers-mode t)
(global-visual-line-mode t)
(show-paren-mode t)
(setq-default fill-column 80)


;; Set up the visible bell
(setq visible-bell t)

;; Automattically follow Symlinks
(setq vc-follow-symlinks t)

;; Disable line numbers for some modes
(dolist (mode '(pdf-view-mode-hook
		term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Start Emacs in fullscreen mode
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
;; Full Screen Frame
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; Set Location Data----------------------------------------------------------------------
(setq calendar-latitude 42.33)
(setq calendar-longitude -83.04)
(setq calendar-location-name "Detroit,MI")

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
;; Emojify 
(use-package emojify)

;; Undo Tree
(use-package undo-tree)
(global-undo-tree-mode 1)

;; Which-key
(use-package which-key
  		   :diminish which-key-mode
  		   :config
  		   (which-key-mode)
  		   (setq which-key-idle-delay 1))

;; Helpful
(use-package helpful 
               	   :commands (helpful-callable helpful-variable helpful-command helpful-key)
                   :bind
                   ([remap describe-command] . helpful-command)
                   ([remap describe-key] . helpful-key))

;; Icons
(use-package all-the-icons)
;; After downloading run "M-x all-the-icons-install-fonts" on first start up.

;; Visual Changes-----------------------------------------------------------------------
;; Font Configuration
(set-face-attribute 'default nil :font "JetBrains Mono"
                       		 :weight 'semibold
				 :height 180)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" 
		    		     :weight 'semibold 
				     :height 180)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Overpass" 
					:weight 'regular
		    			:height 220) 

;; Doom Themes
(use-package doom-themes
  :init (load-theme 'doom-city-lights t))
		   
;; Doom Modline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Keyboard ------------------------------------------------------------------------------------
;; Assign ESC to quite prompts
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-S-v") 'clipboard-yank)

;; Evil
(use-package evil
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
(use-package evil-collection)

;; General
(use-package general
  :after evil
  :config
  (general-create-definer runemacs/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

  (runemacs/leader-keys
   "t"  '(:ignore t :which-key "toggles")))

;; Set Emacs state modes
(dolist (mode '(custom-mode
                eshell-mode
                git-rebase-mode
                term-mode))
        (add-to-list 'evil-emacs-state-modes mode))

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package hydra)
;; Configure this more as workflows develope

;; Fly-X----------------------------------------------------------------------------------
;; FlySpell
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda ()
;; 	(setq ispell-program-name "~/.guix-profile/bin/hunspell")
;; 	(flyspell-mode 1))))
;;TODO Install Flycheck

;; Org------------------------------------------------------------------------------------
;; Turn on indentation and auto-fill mode for Org files
(defun runemacs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (display-line-numbers-mode 0)
  (diminish org-indent-mode))


(defun runemacs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Overpass" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . runemacs/org-mode-setup)
        (org-mode . runemacs/org-font-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t	
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      ;; (ledger . t)
      (lisp . t)
      (latex . t)
      (scheme . t)))
  
  (push '("conf-unix" . conf-unix) org-src-lang-modes))


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (defun runemacs/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . runemacs/org-mode-visual-fill))

;; ;; Latex Mode
;; (use-package tex
;;   :straight auctex)
;; ;; Changes default render engine.
;; (setq latex-run-command "xelatex")
;; (setq org-latex-compiler "xelatex")

;; (use-package latex-preview-pane)

;; (use-package pdf-tools
;;   :defer t
;;   :pin manual
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-width)
;;   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
;;   :bind (:map pdf-view-mode-map
;; 	      ("s" . pdf-occur)
;; 	      ("g" . pdf-view-first-page)
;; 	      ("G" . pdf-view-last-page)
;; 	      ("j" . pdf-view-next-page) 
;; 	      ("k" . pdf-view-previous-page)
;; 	      ("e" . pdf-view-goto-page)
;; 	      ("u" . pdf-view-revert-buffer)
;; 	      ("y" . pdf-view-kill-ring-save)
;; 	      ("m" . pdf-misc-display-metadata)
;; 	      ("b" . pdf-view-set-slice-from-bounding-box)
;; 	      ("r" . pdf-view-reset-slice)
;; 	      ("ad" . pdf-annot-delete)
;; 	      ("aa" . pdf-annot-attachment-dired)
;; 	      ("<s-spc>" . pdf-view-scroll-down-or-next-page))
;;   :custom
;;   (pdf-annot-activate-created-annotations t "automatically annotate highlights")
;;   (pdf-view-active-region nil))

;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;       TeX-source-correlate-start-server t)

;; (add-hook 'TeX-after-compilation-finished-functions
;; 	  #'TeX-revert-document-buffer)

;; (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

;; (use-package org-pdfview
;;   :config
;;   (add-to-list 'org-file-apps
;; 	       '("\\.pdf\\'" . (lambda (file link)
;; 				 (org-pdfview-open link)))))

;; (use-package org-ref
;;   :after helm-bibtex
;;   :init
;;   (require 'bibtex)
;;   (require 'org-ref-helm)
;;   (setq bibtex-autokey-year-length 4
;; 	bibtex-autokey-name-year-separator "-"
;; 	bibtex-autokey-year-title-separator "-"
;; 	bibtex-autokey-titleword-separator "-"
;; 	bibtex-autokey-titlewords 2
;; 	bibtex-autokey-titlewords-stretch 1
;; 	bibtex-autokey-titleword-lenght 5
;; 	bibtex-completion-bibliography '("~/Temp-Archive/Files/global.bib")
;; 	org-ref-insert-link-function 'org-ref-link-hydra/body
;; 	org-ref-insert-cite-function 'org-ref-cite-insert-helm
;; 	org-ref-insert-label-function 'org-ref-insert-label-link
;; 	org-ref-insert-ref-function 'org-ref-insert-ref-link))
;;   ;; (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
;;   ;; (define-key org-mode-map (kbd "s-]") 'org-ref-insert-link-hydra/bod)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Archive/Nodes"))
  (org-roam-completion-everywhere t)
  ;; (org-roam-capture-templates
  ;;  '(("r" "Reference Core" plain
  ;;     (file "~/Temp-Archive/Files/Templates/Reference-Core.org")
  ;; 	    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  ;; 	    :unnarrowed t)
  ;;    ("d" "Default" plain
  ;;     "%?"
  ;; 	    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  ;; 	    :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today))
	 ;; :map org-mode-map
	 ;; ("C-c i c" . completion-at-point)
	 ;; ("C-c i p" . org-insert-link)
  :config
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;;(require 'org-roam-protocol)
  (org-roam-setup))

;; Org Insert Link Keys
;; (define-key bibtex-mode-map (kbd "C-c i c") 'completion-at-point)
;; (define-key bibtex-mode-map (kbd "C-c i r") 'org-ref-insert-link)
;; (define-key org-mode-map (kbd "C-c i l") 'org-insert-link)

;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (require 'org-ref))

;; (use-package org-transclusion
;;   :after org)
;; (define-key global-map (kbd "<f12>") #'org-transclusion-add)

;; Helm and Dependencies
(use-package async)

(use-package dired-async
  :straight async
  :diminish (dired-async-mode)
  :init (setq dire-async-message-function #'message)
  (with-eval-after-load 'dired (dired-async-mode)))
(dired-async-mode 1)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1))

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(use-package helm-bibtex)

;; Anki Integration
(use-package anki-editor)

