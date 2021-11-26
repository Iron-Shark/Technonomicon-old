;;Configure use package and repositories
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package) ;;Requires use-package on startup
(setq use-package-always-ensure t)

;;Basic apperance configuration
(setq inhibit-startup-message t)	;Disables start screen message

(scroll-bar-mode -1)	;Disable visible scroll bar
(tool-bar-mode -1)	;Disable the toolbar
(tooltip-mode -1)	;Disable tooltips
(set-fringe-mode 10)	;Give some breathing room

(setq visible-bell t)	;Enables visual bell

;;Create default font size
(defvar runemacs/default-font-size 180)
;;Configure font
(set-face-attribute 'default nil :font "Overpass" :height runemacs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Overpass" :height 260)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 295 :weight 'regular)

;;Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

