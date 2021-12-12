;; Temporary Theme------------------------------------------------------------------------
(load-theme 'wombat)
;; Install Straight.el--------------------------------------------------------------------
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

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Initialize use-package
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Basic UI Configuration ------------------------------------------------------
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
;; Font Configuration ----------------------------------------------------------
(set-face-attribute 'default nil :font "Overpass Mono" :height runemacs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Overpass Mono" :height 160)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Overpass" :height 195 :weight 'regular)
;; Set Location Data
(setq calendar-latitude 42.3314)
(setq calendar-longitude -83.0458)
(setq calendar-location-name "Detroit,MI")
(column-number-mode)
(global-display-line-numbers-mode t)
