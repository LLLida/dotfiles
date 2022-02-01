;; My init.el  -*- lexical-binding: t; -*-


;;; Defaults
;; Info about me
(setq user-full-name "Adil Mokhammad"
      user-mail-address "0adilmohammad0@gmail.com")

;; setup use-package
(setq package-check-signature nil
      package-native-compile t
      package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("MELPA-STABLE" . "https://stable.melpa.org/packages/")))
(package-initialize)
;; Ensure that we have use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t
      use-package-ignore-unknown-keywords t)

;; set load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

(delete-selection-mode t)

;; say no to backups
(setq auto-save-default nil
      auto-save-list-file-name nil
      make-backup-files nil ;; Don't make backups
      confirm-kill-processes nil ;; don't bother confirming killing processes
      )

;; smooth scrolling
(setq scroll-step 1
      scroll-margin 7
      scroll-conservatively 10000)

;; always type y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; enhance builtin completion
(setq completion-ignore-case t
      completions-detailed t
      read-buffer-completion-ignore-case t
      completion-styles '(substring initials flex partial-completion))

;; 16MB, default value is too small
(setq gc-cons-threshold (* 16 1024 1024))

;; start emacs server
;; so we can just run emacsclient -c in other frames with the same emacs instance
(server-start)

;; Easily switch between .cpp and .hpp files
(global-set-key (kbd "M-o") 'ff-find-other-file)

;; Delete trailing whitespaces and save buffer.
(global-set-key [remap save-buffer] (lambda ()
                                      (interactive)
                                      (delete-trailing-whitespace)
                                      (save-buffer)))

;; save buffers between sessions
(desktop-save-mode 1)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;;; Utilities
;; minimize information in mode line
(use-package diminish)

(require 'lida-keys)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-=" . mc/mark-all-like-this))
  :config
  (setq mc/always-run-for-all t)
  ;; (xah-fly-define-char "a" 'mc/mark-all-like-this)
  )

;; abbrev mode
(require 'abbrev)
(diminish 'abbrev-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
(setq save-abbrevs 'silently)
;; Place @@ in your abbreviation and it will place cursor there!
(defadvice expand-abbrev (after my-expand-abbrev activate)
  (if ad-return-value
      (run-with-idle-timer 0 nil
                           (lambda ()
                             (let ((cursor "@@"))
                               (if (search-backward cursor last-abbrev-location t)
                                   (delete-char (length cursor))))))))

;; mark things very fast
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; dired - the file manager
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; Colorised dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))
;; Collapse directories that only have 1 file
(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; manage popups easily
(use-package shackle
  :config
  (setq shackle-rules '((compilation-mode :noselect t :popup t :align 'below)
                        (eshell-mode :select t :popup t :align 'above :size 0.2)
                        (helpful-mode :select t :popup t :align 'right :size 0.4)
                        (help-mode :select t :popup t :align 'below :size 0.4)
                        ("\\`\\*Warnings.*?\\*\\'" :regexp t :popup t :align 'below :size 0.3)))
  (shackle-mode))

;; display ^L as horizontal lines
(use-package form-feed
  :diminish
  :hook (emacs-lisp-mode . form-feed-mode))

;; setup eshell
(global-set-key (kbd "C-:") #'eshell)
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local mode-line-format nil)))

;; COMPlete ANy
(use-package company
  :demand t
  :diminish
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.5)
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-semantic company-backends))
  (global-company-mode 1)
  ;; disabling company for gdb for now because it just stops responding when there are too many completions
  (add-hook 'gud-mode-hook (lambda ()
                             (company-mode -1))))
(use-package company-quickhelp ;; Show docs within popup
  :after company
  :config (company-quickhelp-mode))

;; displays available keys if you forgot one of them
(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support))

;; documentation
(diminish 'eldoc-mode)
(global-eldoc-mode 1)

;; indent
(global-set-key (kbd "RET") 'newline-and-indent)
(setq-default indent-tabs-mode nil)
(setq c-default-style "gnu")

;; parentheses
(show-paren-mode t)
(setq show-paren-style 'mixed);; highlight brackets if visible, else entire expression
;; (electric-pair-mode t)
;; (electric-indent-mode nil)

;; compilation
(require 'compile)
(setq compilation-ask-about-save nil
      compilation-window-height 14)
;; colored compilation buffer
(use-package xterm-color
  :config
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)
        compilation-scroll-output 'first-error)
  (setq compilation-environment '("TERM=xterm-256color"))
  (advice-add 'compilation-filter :around #'(lambda (f proc string)
                                              (funcall f proc (xterm-color-filter string)))))

;; manage projects
(use-package projectile
  :diminish
  :config
  (add-to-list 'projectile-globally-ignored-directories "build")
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (bind-key "C-c" 'projectile-compile-project projectile-command-map)
  (bind-key "C-s" 'projectile-grep projectile-command-map))

;; ibuffer
(require 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("telega" (or
                          (mode . telega-root-mode)
                          (mode . telega-chat-mode)))
               ("erc" (or
                       (mode . erc-mode)
                       (mode . erc-list-menu-mode)))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (mode . special-mode)
                         (mode . package-menu-mode)
                         (mode . fundamental-mode)
                         (mode . emacs-lisp-compilation-mode)))
               ("org" (mode . org-mode))))))
;; Add icons to ibuffer
(use-package all-the-icons-ibuffer
  :after (all-the-icons ibuffer)
  :config
  (all-the-icons-ibuffer-mode 1)
  (setq all-the-icons-ibuffer-human-readable-size t))

;; show additional information when completing commands
(use-package marginalia
  :config
  (marginalia-mode))


;;; Programming modes

;; syntax highlighting with tree sitter
(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook ((c-mode
          c++-mode c-or-c++-mode
          python-mode
          ;; d-mode
          ) . tree-sitter-hl-mode)
  :config (use-package tree-sitter-langs))

(use-package company-c-headers
  :after company
  :config (add-to-list 'company-backends 'company-c-headers))

;; git
;; (unless use-god-mode
;;   (use-package magit
;;     :commands (magit-status magit)
;;     :bind (:map xah-fly-command-map
;;                 ("<SPC> e g"))))

;; some keybindings for c/c++
(defun c-end-expression ()
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert ";")))
(require 'cc-mode)
(bind-key "C-;" 'c-end-expression c-mode-map)
(bind-key "C-;" 'c-end-expression c++-mode-map)
(defun c-next-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
(bind-key "C-<return>" 'c-next-line c-mode-map)
(bind-key "C-<return>" 'c-next-line c++-mode-map)

;; syntax highlighting for cmake
(use-package cmake-mode)

(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.glsl\\'" . glsl-mode))
  :config
  (bind-key "C-<return>" 'c-next-line glsl-mode-map))

(use-package d-mode
  :mode ("\\.d\\'" . d-mode)
  :config
  (bind-key "C-<return>" 'c-next-line d-mode-map)
  (bind-key "C-;" 'c-end-expression d-mode-map))

;; view pdf files
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config (pdf-tools-install))


;;; UI

;; Icons
(when (display-graphic-p)
  (use-package all-the-icons
    :config
    (when (display-graphic-p)
      (unless (member "all-the-icons" (font-family-list))
        (message "fonts for all-the-icons are not installed, "
                 "please, install them via `all-the-icons-install-fonts'."))))
  ;; Add icons to dired
  (use-package all-the-icons-dired
    :diminish
    :hook (dired-mode . all-the-icons-dired-mode)
    :config (setq all-the-icons-dired-monochrome nil))
  )

;; Modeline
(use-package smart-mode-line
  :config
  (setq sml/name-width 30
        sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (sml/setup))
(setq inhibit-compacting-font-caches t
      mode-line-position nil
      display-time-default-load-average nil
      display-time-format " %R ")
(setq battery-mode-line-format "(%b%p%%)")
(display-battery-mode t)
(display-time)
;; cull mode which displays current function name in modeline
(which-function-mode 1)

;; theme
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t))

;; font
(set-frame-font "DejaVu Sans Mono 11" t t)

;; tabs
(tab-bar-mode)
;; highlight current tab
(set-face-attribute 'tab-bar-tab nil
                    :foreground nil
                    :background nil
                    :box t
                    :inherit 'font-lock-keyword-face)
;; change tab format
(defun lida/tab-bar-format (tab i)
  (propertize
   (alist-get 'name tab)
   'face (funcall tab-bar-tab-face-function tab)))
(setq tab-bar-tab-name-format-function #'lida/tab-bar-format)
;; pg up/down switch between tabs
(progn
  (require 'god-mode)
  (bind-key "<prior>" 'tab-bar-switch-to-prev-tab god-local-mode-map)
  (bind-key "<next>" 'tab-bar-switch-to-next-tab god-local-mode-map))
;; move global data in modeline(such as time or battery status) to tab bar.
(setq lida/global-mode-string '("" display-time-string battery-mode-line-string))
(defun lida/tab-bar-format-global ()
  "Produce display of `global-mode-string' in the tab bar.
When `tab-bar-format-global' is added to `tab-bar-format'
(possibly appended after `tab-bar-format-align-right'),
then modes that display information on the mode line
using `global-mode-string' will display the same text
on the tab bar instead."
  `((global menu-item ,(string-trim-right (format-mode-line lida/global-mode-string)) ignore)))
(setq tab-bar-format '(tab-bar-format-history
                       tab-bar-format-tabs
                       tab-bar-format-align-right
                       lida/tab-bar-format-global))
(setq global-mode-string '(""))


;;; Misc

;; telegram
(use-package telega
  :bind-keymap ("M-t" . telega-prefix-map)
  :config
  ;; (setq telega-completing-read-function 'ivy-completing-read) ;; use ivy
  (setq telega-completing-read-function 'completing-read) ;; use builtin completion
  (require 'telega-mnz)
  (global-telega-mnz-mode t)
  (require 'telega-stories)
  (telega-stories-mode t)
  (defun my-telega-chat-mode ()
    (set (make-local-variable 'company-backends)
         (append (list 'telega-company-emoji
                       'telega-company-username
                       'telega-company-hashtag)
                 (when (telega-chat-bot-p telega-chatbuf--chat)
                   '(telega-company-botcmd))))
    (company-mode 1))
  (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode))
;; don't forget to 'yay -S ttf-symbola'!
(set-fontset-font t 'unicode "Symbola" nil 'append)

;; mail with mu4e(don't forget to do 'yay -S mu'!)
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-03.org
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :bind (:map xah-fly-command-map
  ;;             ("<SPC> e m" . mu4e))
  :config
  (setq mu4e-change-filenames-when-moving t
        mu4e-update-interval 1800
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/Mail")
  (setq mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-sent-folder "/[Gmail]/Sent Mail"
        mu4e-refile-folder "/[Gmail]/All Mail"
        mu4e-trash-folder "/[Gmail]/Trash")
  (setq mu4e-maildir-shortcuts
        '(("/Inbox" . ?i)
          ("/[Gmail]/Sent Mail" . ?s)
          ("/[Gmail]/Trash" . ?t)
          ("/[Gmail]/Drafts" . ?d)
          ("/[Gmail]/All Mail" . ?a)))
  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl)
  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it))

;; Org mode
(setq org-catch-invisible-edits 'smart)
;; display current location in org document in headerline
(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq org-sticky-header-full-path 'full
        org-sticky-header-outline-path-separator "->"
        org-sticky-header-heading-star ""
        org-sticky-header-show-priority nil
        org-sticky-header-show-keyword nil))
;; Replace ... for hidden items with ↴
(setq org-ellipsis " ↴")
;; Toggle emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t))

;; music in emacs
(require 'lida-music)

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
