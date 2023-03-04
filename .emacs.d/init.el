;; My init.el  -*- lexical-binding: t; -*-


;;; Defaults
;; Info about me
(setq user-full-name "Adil Mokhammad"
      user-mail-address "0adilmohammad0@gmail.com")

;; setup use-package
(setq package-check-signature nil
      package-native-compile t
      package-enable-at-startup nil
      package-quickstart t)
;; set package archives
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
      scroll-margin 1
      scroll-conservatively 10000)

;; always type y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; enhance builtin completion
(setq completion-ignore-case t
      completions-detailed t
      read-buffer-completion-ignore-case t
      completion-styles '(basic substring flex)
      ;; from article https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html
      completions-format 'one-column
      completions-max-height 40)
(bind-key "C-n" 'minibuffer-next-completion minibuffer-mode-map)
(bind-key "C-p" 'minibuffer-previous-completion minibuffer-mode-map)

;; default value is too small
(setq gc-cons-threshold (* 8 1024 1024))

(setq undo-limit (* 8 1024 1024))

;; start emacs server
;; so we can just run emacsclient -c in other frames with the same emacs instance
(server-start)

;; Easily switch between .cpp and .hpp files
(global-set-key (kbd "M-o") 'ff-find-other-file)

;; Delete trailing whitespaces before save buffer.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; save minibuffer history
(savehist-mode 1)

;; disable suspend-frame bindings as I sometimes hit them accidentally
(global-set-key (kbd "C-z") #'repeat)
(global-set-key (kbd "C-x C-z") nil)

;; bind some search-replace commands to easier keybindings
(bind-key "M-1" #'query-replace)
(bind-key "M-2" #'replace-string)

;; very useful
(bind-key "M-z" #'pop-to-mark-command)

(bind-key "C-x C-b" #'buffer-menu)

;; do not confirm when killing buffer
(bind-key [remap kill-buffer] #'kill-this-buffer)


;;; Utilities

;; minimize information in mode line
(use-package diminish)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-=" . mc/mark-all-like-this))
  :config
  (setq mc/always-run-for-all t)
  )

;; hippie-expand
(require 'hippie-exp)
;; most of the time I use "M-/" (dabbrev-expand), but I happen to use hippie-expand too
(global-set-key (kbd "C-/") #'hippie-expand)
(setq hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-list
                                         try-expand-dabbrev-from-kill
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(add-to-list 'hippie-expand-ignore-buffers 'archive-mode)
(add-to-list 'hippie-expand-ignore-buffers 'image-mode)

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
(add-hook 'dired-mode-hook 'hl-line-mode)
;; took from https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-dired.el
(setq dired-recursive-copies 'always
      dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
;; Emacs 29
(setq dired-make-directory-clickable t
      dired-free-space nil
      dired-mouse-drag-files t)
(bind-key "C-x v v" #'dired-vc-next-action dired-mode-map)

;; Collapse directories that only have 1 file
(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; use tab to expand directory at point
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-cycle)))

;; proced - Emacs process manager
;; https://laurencewarne.github.io/emacs/programming/2022/12/26/exploring-proced.html
(use-package proced
  :ensure nil
  :commands proced
  :config
  ;; this significantly slows Emacs if proced buffer stays open
  ;; (setq-default proced-auto-update-flag t)
  (setq proced-goal-attribute nil
        proced-enable-color-flag t)
  (setq-default proced-format 'long))

;; display ^L as horizontal lines
(use-package form-feed
  :diminish
  :hook (emacs-lisp-mode . form-feed-mode))

;; setup eshell
(global-set-key (kbd "C-:") #'project-eshell)

;; displays available keys if you forgot one of them
(use-package which-key
  :diminish
  :config
  (which-key-mode))

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

;; colored compilation buffer
(use-package xterm-color
  :config
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)
        compilation-scroll-output 'first-error)
  (setq compilation-environment '("TERM=xterm-256color"))
  (advice-add 'compilation-filter :around #'(lambda (f proc string)
                                              (funcall f proc (xterm-color-filter string)))))

;; ibuffer
(require 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda ()
                               (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("C/C++" (or
                         (mode . c-mode)
                         (mode . c++-mode)
                         (mode . glsl-mode)))
               ("dired" (mode . dired-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (mode . special-mode)
                         (mode . package-menu-mode)
                         (mode . fundamental-mode)
                         (mode . emacs-lisp-compilation-mode)))
               ("telega" (or
                          (mode . telega-root-mode)
                          (mode . telega-chat-mode)))
               ("org" (mode . org-mode))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("erc" (or
                       (mode . erc-mode)
                       (mode . erc-list-menu-mode)))
               ))))


;;; Programming modes

;; git
(use-package magit
  :commands (magit-status magit))

;; highlight TODO keywords, hl-todo package seems very slow.
;; We're using minad's suggestion instead
;; https://github.com/tarsius/hl-todo/issues/61
(defvar my/todo-keywords
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("NOTE"   . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "#cc9393")
    ))
(defun my/todo-fontify ()
  (unless (derived-mode-p 'org-mode)
    (font-lock-add-keywords
     nil
     (mapcar (lambda (x)
               `(,(concat "\\<" (car x) "\\>") 0 '(:weight bold :foreground ,(cdr x)) prepend))
             my/todo-keywords))))
(add-hook 'prog-mode-hook #'my/todo-fontify)

;; Greate article for setting Emacs for C/C++
;; https://tuhdo.github.io/c-ide.html

;; improve syntax highlighting in C-based modes
;; https://emacs.stackexchange.com/questions/16750/better-syntax-higlighting-for-member-variables-and-function-calls-in-cpp-mode
(dolist (mode-iter '(c-mode c++-mode glsl-mode))
  ;; constants
  (font-lock-add-keywords
    mode-iter
    '(("\\<\[A-Z0-9_\]\+\\>" 0 'font-lock-constant-face keep)) t)
  ;; functions
  (font-lock-add-keywords
    mode-iter
    '(("\\([_a-zA-Z][_a-zA-Z0-9]*\\)\s*(" 1 'font-lock-function-name-face keep)) t))

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

;; ggtags
(use-package ggtags
  :diminish
  :bind (("M-s C-g" . ggtags-mode))
  :hook (ggtags-mode-hook . (lambda ()
                              (eldoc-mode 1)))
  :config
  (add-to-list 'hippie-expand-try-functions-list 'ggtags-try-complete-tag t))

(defun my/smart-insert-parens (begin end)
  "Insert parens around marked region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (save-excursion
    (goto-char begin)
    (insert "(")
    (goto-char (+ 1 end))
    (insert ")"))
  (unless (use-region-p)
    (forward-char)))
(bind-key "C-(" 'my/smart-insert-parens)

;; syntax highlighting for cmake
(use-package cmake-mode)

(use-package glsl-mode
  :mode (("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)
         ("\\.glsl\\'" . glsl-mode))
  :config
  (bind-key "C-<return>" 'c-next-line glsl-mode-map))

;; view pdf files
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install)
  (require 'dabbrev)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'hippie-expand-ignore-buffers 'pdf-view-mode))


;;; UI

;; Modeline
(setq inhibit-compacting-font-caches t
      display-time-default-load-average nil
      display-time-format " %R ")
(setq battery-mode-line-format "(%b%p%%)")
(display-battery-mode t)
(display-time)

;; cool mode which displays current function name in modeline
;; (which-function-mode 1)

;; theme
;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-galaxy t)))
(use-package ef-themes
  :config
  (load-theme 'ef-bio t))
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp")
;; (load-theme 'naysayer t)
;; (load-theme 'modus-vivendi t)

;; font
(set-frame-font "DejaVu Sans Mono 11" t t)

;; tabs
(tab-bar-mode)
;; change tab format
(defun lida/tab-bar-format (tab i)
  (propertize
   (alist-get 'name tab)
   'face (funcall tab-bar-tab-face-function tab)))
(setq tab-bar-tab-name-format-function #'lida/tab-bar-format)
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

;; https://www.emacswiki.org/emacs/WholeLineOrRegion
(defun my/kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
          (goto-char end)
        (goto-char beg)))))
(global-set-key [remap kill-ring-save] 'my/kill-ring-save)
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))


;;; Misc

;; telegram
(use-package telega
  :bind-keymap ("M-t" . telega-prefix-map)
  :config
  (setq telega-completing-read-function 'completing-read) ;; use builtin completion
  (require 'telega-mnz)
  (global-telega-mnz-mode t)
  (require 'telega-stories)
  (telega-stories-mode t))
;; don't forget to 'yay -S ttf-symbola'!
(set-fontset-font t 'unicode "Symbola" nil 'append)

;; mail with mu4e(don't forget to do 'yay -S mu'!)
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-03.org
(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :commands (mu4e)
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
