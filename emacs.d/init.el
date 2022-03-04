(setq package-enable-at-startup nil)

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

(setq straight-repository-branch "develop")

(straight-use-package
 '(use-package
    :type git
    :host github
    :repo "jwiegley/use-package"))

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/straight/build")
  (require 'use-package))

(setq straight-use-package-by-default t) ;Adds the straight argument to all use-package statements.
(setq use-package-always-ensure t) ;Adds the require argument to all use-package statements.

(scroll-bar-mode -1) ;Removes scrollbars from UI.
(tool-bar-mode -1) ;Removes toolbar from UI.
(set-fringe-mode 5) ;Adds spacing from edge of frame.
(menu-bar-mode -1) ;Removes menue bar from UI.
(column-number-mode 1) ;Adds column number to minibuffer.
(show-paren-mode t) ;Highlight paren currently under point.
(global-display-line-numbers-mode t) ;Adds line numbers to buffers by default.
(global-visual-line-mode t) ;Enables visual line wrapping in buffers.
(setq-default fill-column 80) ; Visual line wrap after 80 characters.
(setq inhibit-startup-message t) ; Disables default landing screen, scratch buffer used instead.
(setq visible-bell t    ; Enables visual alert bell.
      ring-bell-function 'ignore) ; Disable sound bell.

(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                shell-mode-hook
                eww-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default initial-scratch-message nil)

(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-format "%Y-%m-%d %H:%M")

(display-time-mode 1)

(global-auto-revert-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode 1)

(setq vc-follow-symlinks t) ;Follows symlinks with out prompting user.

(setq calendar-latitude 42.33
      calendar-longitude -83.04
      calendar-location-name "Detroit,MI"
      user-full-name "Que Fanning"
      user-real-login-name "Que Fanning"
      user-login-name "Que"
      user-mail-address "Que@ironshark.org")

;(custom-set-variables
; '(initial-frame-alist (quote ((fullscreen . maximized)))))
;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(defvar technonomicon/default-font-size 150)

(set-face-attribute 'default nil
                    :font "Fira Code"
                    :weight 'semibold
                    :height 180)

(set-face-attribute 'fixed-pitch nil
                    :font "Fira Code"
                    :weight 'semibold
                    :height 180)

(set-face-attribute 'variable-pitch nil
                    :font "Fira Sans"
                    :weight 'light
                    :height 220)



(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default sentence-end-double-space nil)

(setq custom-file "~/Voyager-Config/emacs.d/custom.el")
(load custom-file)

(setq backup-directory-alist '(("." . "~/Neuromancer/Archive/Files/Emacs-Bak"))
      backup-by-copying t
      version-control t
      vc-make-backup-files t
      kept-old-versions 5
      kept-new-versions 20
      delete-old-versions t
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
        '(kill-ring
             search-ring
             regexp-search-ring))


(setq tramp-backup-directory-alist backup-directory-alist
      auto-save-file-name-transforms '((".*" "~/Neuromancer/Archive/Files/Emacs-Bak/Auto-Saves" t))
      savehist-file "~/Neuromancer/Archive/Files/Emacs-Bak/Save-hist")

(savehist-mode 1)

(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-city-lights t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package emojify)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                  ; (setq ispell-program-name "~/.guix-profile/bin/hunspell")
                   (flyspell-mode 1))))

(use-package undo-tree)
(global-undo-tree-mode 1)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)

;(defun revert-buffer-no-confirm ()
;  (interactive) (revert-buffer t t))

;(define-key global-map (kbd "C-u u b") 'revert-buffer-no-confirm)

(use-package evil
  :init
  (setq evil-want-integration t
       evil-want-keybinding nil
       evil-want-C-u-scroll t
       evil-want-C-i-jump nil
       evil-respect-visual-line-mode t
       evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Sets the starting EVIL state for certain modes.
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(use-package evil-collection)

(dolist (mode '(custom-mode
                   eshell-mode
                   git-rebase-mode
                   term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(use-package general
  :after evil
  :config
  (general-create-definer technonomicon/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (technonomicon/leader-keys
    "t" '(:ignore t :which-key "toggles")))

(use-package hydra)

(defun technonomicon/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (display-line-numbers-mode 0)
  (setq evil-auto-indent nil
             org-src-preserve-indentation nil
             org-edit-src-content-indentation 0))

(defun technonomicon/org-font-setup ()

(font-lock-add-keywords 'org-mode
                        '(("^*\\([-])\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                      (org-level-2 . 1.1)
                      (org-level-3 . 1.05)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.1)
                      (org-level-6 . 1.1)
                      (org-level-7 . 1.1)
                      (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Overpass" :weight 'regular :height (cdr face)))

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

:hook (org-mode . technonomicon/org-mode-setup)
      (org-mode . technonomicon/org-font-setup)

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
      org-confirm-babel-evaluate nil
      org-capture-bookmark nil)

(evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
(evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

(evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (lisp . t)
   (latex . t)
   (scheme . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes))

(setq org-directory "~/Neuromancer/Grimoire/Org"
      org-agenda-files '("~/Neuromancer"
                              "~/Projects"))

;; (define-key org-mode-map (kbd "C-c i c") 'completion-at-point)
;; (define-key org-mode-map (kbd "C-c i r") 'org-ref-insert-link)
;; (define-key org-mode-map (kbd "C-c i l") 'org-insert-link)
;; (define-key org-mode-map (kbd "C-c i t") 'org-transclusion-add)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(add-hook 'bibtex-mode-hook 'flyspell-mose)
(setq bibtex-user-optional-fields '(("keywords" "Search keywords" "")
                                          ("file" "Link to source file" ":")
                                          ("Summary" "Summary of source" ""))
      bibtex-align-at-equal-sign t
      bibtex-dialect 'biblatex)

;(setq bib-files-directory pathe/to/directory)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-transclusion
  :after org)
(define-key global-map (kbd "<f12>") #'org-transclusion-add)

(defun technonomicon/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
               visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . technonomicon/org-mode-visual-fill))

(use-package tex
  :straight auctex)

(setq latex-run-command "xelatex"
      org-latex-compiler "xelatex")

(use-package latex-preview-pane)

(use-package org-pdfview
  :config
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open-link)))))

(use-package pdf-tools
  :defer t
  :pin manual
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)

:bind (:map pdf-view-mode-map
              ("s" . pdf-occur)
              ("g" . pdf-view-first-page)
              ("G" . pdf-view-last-page)
              ("j" . pdf-view-next-page)
              ("k" . pdf-view-previous-page)
              ("e" . pdf-view-goto-page)
              ("u" . pdf-view-revert-buffer)
              ("y" . pdf-view-kill-ring-save)
              ("m" . pdf-misc-display-metadata)
              ("b" . pdf-view-set-slice-from-bounding-box)
              ("r" . pdf-view-reset-slice)
              ("ad" . pdf-annot-delete)
              ("aa" . pdf-annot-attachment-dired)
              ("<s-spc>" . pdf-view-scroll-down-or-next-page))
:custom
(pdf-annot-activate-created-annotations t "automatically annotate highlights")
(pdf-view-active-region nil))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(use-package ox-haunt)
(with-eval-after-load 'ox
  (require 'ox-haunt))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/Neuromancer/Grimoire/Nodes"))
  (org-roam-completion-everywhere t)
  ;; (org-roam-capture-templates
  ;;  '(("r" "Reference Core" plain
  ;;     (file "~/Temp-Archive/Files/Templates/Reference-Core.org")
  ;;	    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  ;;	    :unnarrowed t)
  ;;    ("d" "Default" plain
  ;;     "%?"
  ;;	    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
  ;;	    :unnarrowed t)))
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
  (setq org-roam-node-dispaly-template (concat "${title:*} " (propertize "${tags:10" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  (org-roam-setup))

(setq org-roam-dailies-directory "Journal")

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package org-fc
  :straight
  (org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org"))
  :custom
  (org-fc-directories '("~/Archive/Nodes/"
                        "~/Archive/Files/"
                        "~/Projects"))
  :config
  (require 'org-fc-hydra))

(evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
  (kbd "RET") 'org-fc-review-flip
  (kbd "n") 'org-fc-review-flip
  (kbd "s") 'org-fc-review-suspend-card
  (kbd "q") 'org-fc-review-quit)

(evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
  (kbd "a") 'org-fc-review-rate-again
  (kbd "h") 'org-fc-review-rate-hard
  (kbd "g") 'org-fc-review-rate-good
  (kbd "e") 'org-fc-review-rate-easy
  (kbd "s") 'org-fc-review-rate-suspend-card
  (kbd "q") 'org-fc-review-quit)

(use-package org-ref
  :after helm-bibtex ; Initializes org-ref after helm-bibtex has loaded
  :init
  (require 'bibtex) ; Requires bibtex org sub-module
  (require 'org-ref-helm) ; Requires the helm sub-module of Org-ref
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-lenght 5
        bibtex-completion-bibliography '("~/Archive/Files/Global/Bibliography.bib")
        org-ref-insert-link-function 'org-ref-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link))

;; (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
;; (define-key org-mode-map (kbd "s-]") 'org-ref-insert-link-hydra/body)

(use-package async)

(use-package dired-async
  :straight async
  :diminish (dired-async-mode)
  :init (setq dired-async-message-function #'message)
  (with-eval-after-load 'dired (dired-async-mode)))

(dired-async-mode 1)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1))

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(use-package swiper-helm)

(use-package company
  :bind (("C-c ." . company-complete)))

(setq company-tooltip-limit 10
      company-show-numbers t
      company-idle-delay 0.3
      company-echo-delay 0)



(add-hook 'after-init-hook 'global-company-mode)

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode))

(global-company-fuzzy-mode 1)
