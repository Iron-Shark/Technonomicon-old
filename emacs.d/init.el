(setq inhibit-startup-message t)	;Disabls start screen message

(scroll-bar-mode -1)	;Disable visible scroll bar
(tool-bar-mode -1)	;Disable the toolbar
(tooltip-mode -1)	;Disable tooltips
(set-fringe-mode 10)	;Give some breathing room

(setq visible-bell t)	;Enables visual bell

;;TODO update default font height and type
(load-theme 'tango-dark)

;;Initialize Package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;(unless (package-installed-p 'use-package)
;  (package-refresh-contents)

(require 'use-package)
(setq use-package-always-ensure t)


