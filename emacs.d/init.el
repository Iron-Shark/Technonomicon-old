;;;; This file is generated automatically from emacs.org, make any changes there.

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

(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-format "%Y-%m-%d %H:%M")

(display-time-mode 1)
(column-number-mode 1) ; Adds column number to modeline

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(menu-bar-mode -1) ; Removes menue bar from UI.
(tool-bar-mode -1) ; Removes toolbar from UI.
(scroll-bar-mode -1) ; Removes scrollbars from UI.
(set-fringe-mode 5) ; Adds spacing from edge of frame.
(global-hl-line-mode 1) ; Highlights line currently under point.
(setq org-startup-with-inline-images t) ; Displays org-mode image previews.

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

(use-package all-the-icons)

(use-package emojify)

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

(use-package doom-themes
  :init (load-theme 'doom-city-lights t))

(global-display-line-numbers-mode t) ;Adds line numbers to buffers by default.

;;; Disables line numbers in listed modes
(dolist (mode '(pdf-view-mode-hook
                term-mode-hook
                shell-mode-hook
                eww-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default initial-scratch-message nil) ; Removes default message from scratch buffer.
(setq inhibit-startup-message t ; Disables default landing screen, scratch buffer used instead.
      initial-buffer-choice "~/Neuromancer/splash.org") ; Creates custom landing buffer.

(setq calendar-latitude 42.33
      calendar-longitude -83.04
      calendar-location-name "Detroit,MI"
      user-full-name "Que Fanning"
      user-real-login-name "Que Fanning"
      user-login-name "Que"
      user-mail-address "Que@ironshark.org")

;;; Local File Versioning
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

;;; Custom File Declaration
(setq custom-file "~/Voyager-Config/emacs.d/custom.el")
(load custom-file)

;;; History and Message Buffer Tracking
(setq history-delete-duplicates t
      history-length            100 ; default is 30.
      report-emacs-bug-no-explanations t
      comint-prompt-read-only          t
      uniquify-buffer-name-style       nil
      register-preview-delay           nil
      message-log-max                  1000
      kill-ring-max                    100
      mark-ring-max                    100
      global-mark-ring-max             200)

(global-visual-line-mode t) ;Enables visual line wrapping in buffers.
(setq-default fill-column 80) ; Visual line wrap after 80 characters.
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;adds visual line wrap indicator.

;;; Remove trailing white space
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default sentence-end-double-space nil)

;;; Automatically updates buffer if file chages on disk.
(global-auto-revert-mode 1)

;;; Changes yes or no mini-buffer prompts to y or n.
(fset 'yes-or-no-p 'y-or-n-p)

;;; Follow Symlinks without prompting user.
(setq vc-follow-symlinks t)

(setq visible-bell t    ; Enables visual alert bell.
      ring-bell-function 'ignore) ; Disable sound bell.

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

(use-package evil-collection)

(use-package undo-tree)
(global-undo-tree-mode 1)

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

;;; Highlight paren currently under point.
(show-paren-mode t)

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1))

;;; Helm Keybindings
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-c h") #'helm-command-prefix)

(use-package swiper-helm)
(global-set-key (kbd "C-s") 'swiper-helm)
(global-set-key (kbd "C-M-s") 'helm-regexp)

(use-package company
  :bind (("C-c ." . company-complete)))

(setq company-tooltip-limit 10
      company-show-numbers t
      company-idle-delay 0.3
      company-echo-delay 0)

;;; Disable Company Mode in listed modes.
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eww-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (company-mode 0))))

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode))

(global-company-fuzzy-mode 1)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
                  ; (setq ispell-program-name "~/.guix-profile/bin/hunspell")
                   (flyspell-mode 1))))

(defun technonomicon/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
               visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . technonomicon/org-mode-visual-fill))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun technonomicon/org-mode-setup ()
  (org-indent-mode 1)
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
  (set-face-attribute (car face) nil :font "Fira Sans" :weight 'regular :height (cdr face)))

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
        (before-save . Tn/org-set-last-modified)
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
;;; add (ledger .t) once leger cli is installed.

(push '("conf-unix" . conf-unix) org-src-lang-modes))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun Tn/org-find-time-file-property (property &optional anywhere)
    "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
        (when (re-search-forward (format "^#\\+%s:" property)
                                 (if anywhere nil first-heading)
                                 t)
          (point)))))

  (defun Tn/org-has-time-file-property-p (property &optional anywhere)
    "Return the position of time file PROPERTY if it is defined.
As a special case, return -1 if the time file PROPERTY exists but
is not defined."
    (when-let ((pos (Tn/org-find-time-file-property property anywhere)))
      (save-excursion
        (goto-char pos)
        (if (and (looking-at-p " ")
                 (progn (forward-char)
                        (org-at-timestamp-p 'lax)))
            pos
          -1))))

  (defun Tn/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.
When ANYWHERE is non-nil, search beyond the preamble.
If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (Tn/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun Tn/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (Tn/org-set-time-file-property "LAST_MODIFIED")))

(use-package tex
  :straight auctex)

(setq latex-run-command "xelatex"
      org-latex-compiler "xelatex")

(use-package latex-preview-pane)

(use-package slime)

(setq inferior-lisp-program "sbcl")

(setq bibtex-user-optional-fields '(("keywords" "Search keywords" "")
                                    ("file" "Link to source file" ":")
                                    ("Summary" "Summary of source" ""))
      bibtex-align-at-equal-sign t
      bibtex-dialect 'biblatex
      bibtex-maintain-sorted-entries t
      bibtex-autokey-edit-before-use t
      bibtex-autokey-before-presentation-hook t
      bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-lenght 5
      bibtex-completion-bibliography '("~/Neuromancer/Grimoire/Files/Globals/Bibliography.bib"))

(defun bibtex-global-view ()
"Opens global bibliography file"
  (interactive)
  (find-file "~/Neuromancer/Grimoire/Files/Globals/Bibliography.bib"))

(define-key org-mode-map (kbd "C-c B") #'bibtex-global-view)

(use-package helm-bibtex)

(setq bibtex-completion-bibliography '("~/Neuromancer/Grimoire/Files/Globals/Bibliography.bib")
      bibtex-completion-library-path '("~/Library")
      bibtex-completion-pdf-field "file"
      bibtex-completion-notes-path "~/Neuromancer/Grimoire/Nodes"
      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-symbol "⌘"
      bibtex-completion-notes-symbol "✎"
      bibtex-completion-pdf-extension '(".pdf" ".djvu" ".jpg")) ;add extensions as needed.

(require 'helm-config)

(define-key helm-command-map "b" 'helm-bibtex)
(define-key helm-command-map "B" 'helm-bibtex-with-local-bibliography)
(define-key helm-command-map "n" 'helm-bibtex-with-notes)
(define-key helm-command-map (kbd "<menu>") 'helm-resume)

;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (setq orb-preformat-keywords '("citekey" "author" "date")))

(use-package org-ref
  :after helm-bibtex ; Initializes org-ref after helm-bibtex has loaded
  :init
  (require 'bibtex) ; Requires bibtex org sub-module
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-lenght 5
        bibtex-completion-bibliography '("~/Neuromancer/Grimoire/Files/Globals/Bibliography.bib"))
  (require 'org-ref-helm)
  (setq org-ref-insert-link-function 'org-ref-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
           org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)
(define-key org-mode-map (kbd "C-c b") 'org-ref-insert-link)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  ;; (require 'org-roam-protocol)
  :custom
        (org-roam-db-update-on-save t) ; May need to be disable for performance
        (org-roam-completion-everywhere t)
        (org-roam-directory "~/Neuromancer/Grimoire/Nodes")
        (org-roam-dailies-directory "Journal/")
        (org-roam-dailes-capture-templates
        '(("d" "Journal" plain
           (file "~/Neuromancer/Grimoire/Files/Templates/journal.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))

       (org-roam-capture-templates
       '(("l" "Literature Note Default" plain
          (file "~/Neuromancer/Grimoire/Files/Templates/litterature-default.org")
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)

         ("r" "Reference Summary" plain
          (file "~/Neuromancer/Grimoire/Files/Templates/reference-default.org")
          :if-new (file+head "references/${citekey}.org" "#+title: ${title}\n")
          :unarrowed t)

;;          ("r" "bibliography reference" plain
;;          "%?
;; %^{author} published %^{entry-type} in %^{date}: fullcite:%\\1."
;;          :target
;;          (file+head "references/${citekey}.org" "#+title: ${title}\n")
;;          :unnarrowed t)

         ("s" "Zettle Default" plain
          (file "~/Neuromancer/Grimoire/Files/Templates/zettle-default.org")
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)

         ("d" "Default" plain
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)))

  :config
       (org-roam-db-autosync-mode)
       (org-roam-setup))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(setq org-roam-node-dispaly-template (concat "${title:*} " (propertize "${tags:10" 'face 'org-tag)))

;;; Set sub-dirctory for Roam Journal entries
(setq org-roam-dailies-directory "Journal")

(global-set-key (kbd "C-c n") org-roam-mode-map)

(defvar org-roam-mode-map (make-sparse-keymap)
  "Prefix key for all Roam actions.")


(define-key org-roam-mode-map (kbd "b") 'org-roam-buffer-toggle)
(define-key org-roam-mode-map (kbd "g") 'org-roam-graph)
(define-key org-roam-mode-map (kbd "i") 'org-roam-node-insert)
(define-key org-roam-mode-map (kbd "c") 'org-roam-capture)

(defvar org-roam-meta-data-map (make-sparse-keymap)
  "Prefix key for adding or removing Roam Meta-data.")
(define-key org-roam-mode-map (kbd "a") org-roam-meta-data-map)

(define-key org-roam-meta-data-map (kbd "a") 'org-roam-alias-add)
(define-key org-roam-meta-data-map (kbd "A") 'org-roam-alias-remove)
(define-key org-roam-meta-data-map (kbd "t") 'org-roam-tag-add)
(define-key org-roam-meta-data-map (kbd "T") 'org-roam-tag-remove)
(define-key org-roam-meta-data-map (kbd "r") 'org-roam-ref-add)
(define-key org-roam-meta-data-map (kbd "R") 'org-roam-ref-remove)

(defvar org-roam-search-map (make-sparse-keymap)
  "Prefix key for searching based on tags.")
(define-key org-roam-mode-map (kbd "f") org-roam-search-map)

(define-key org-roam-search-map (kbd "n") 'org-roam-node-find)

(define-key org-roam-search-map (kbd "c") 'find-core-tag)
(define-key org-roam-search-map (kbd "l") 'find-litterature-tag)
(define-key org-roam-search-map (kbd "r") 'find-reference-tag)
(define-key org-roam-search-map (kbd "n") 'find-noun-tag)
(define-key org-roam-search-map (kbd "i") 'find-index-tag)
(define-key org-roam-search-map (kbd "v") 'find-void-tag)
(define-key org-roam-search-map (kbd "f") 'find-file-tag)

(defun core-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "core" tags)))

(defun find-core-tag ()
  (interactive)
  (org-roam-node-find t nil 'core-search))

(defun litterature-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "lit" tags)))

(defun find-litterature-tag ()
  (interactive)
  (org-roam-node-find t nil 'litterature-search))

(defun reference-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "ref" tags)))

(defun find-reference-tag ()
  (interactive)
  (org-roam-node-find t nil 'reference-search))

(defun noun-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "noun" tags)))

(defun find-noun-tag ()
  (interactive)
  (org-roam-node-find t nil 'noun-search))

(defun index-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "index" tags)))

(defun find-index-tag ()
  (interactive)
  (org-roam-node-find t nil 'index-search))

(defun void-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "void" tags)))

(defun find-void-tag ()
  (interactive)
  (org-roam-node-find t nil 'void-search))

(defun file-search (node)
  (interactive)
  (let ((tags (org-roam-node-tags node)))
    (member "file" tags)))

(defun find-file-tag ()
  (interactive)
  (org-roam-node-find t nil 'file-search))

(defvar org-roam-journal-map (make-sparse-keymap)
  "Prefix key for Roam journal actions.")
(define-key org-roam-mode-map (kbd "j") org-roam-journal-map)


(define-key org-roam-mode-map (kbd "j") 'org-roam-dailies-capture-today)

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

(use-package org-transclusion
  :after org)
(define-key org-mode-map (kbd "<f12>") #'org-transclusion-add)

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

(use-package async)

(use-package dired-async
  :straight async
  :diminish (dired-async-mode)
  :init (setq dired-async-message-function #'message)
  (with-eval-after-load 'dired (dired-async-mode)))

(dired-async-mode 1)
