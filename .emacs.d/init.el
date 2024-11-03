;; My init.el  -*- lexical-binding: t; -*-


;;; Defaults
;; Info about me
(setq user-full-name "Adil Mokhammad"
      user-mail-address "0adilmohammad0@gmail.com")

;; setup package.el
(setq package-check-signature nil
      package-enable-at-startup nil)
(package-initialize)
;; set package archives
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
    ("MELPA"        . "https://melpa.org/packages/")
    ("MELPA-STABLE" . "https://stable.melpa.org/packages/")
    ("NON GNU ELPA" . "https://elpa.nongnu.org/nongnu/")))
(setq use-package-verbose t
      use-package-ignore-unknown-keywords t)

;; set load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

(blink-cursor-mode -1)

(delete-selection-mode t)

;; say no to backups
(setq auto-save-default nil
      auto-save-list-file-name nil
      make-backup-files nil ;; Don't make backups
      confirm-kill-processes nil ;; don't bother confirming killing processes
      )

;; disable creating lock files when editing with Tramp. This makes editing over ssh significantly fast
(defun my-disable-tramp-lockfiles ()
  "Disable lockfiles for tramp."
  (require 'tramp)
  (when (tramp-tramp-file-p (buffer-file-name))
    (setq-local create-lockfiles nil)))
(add-hook 'find-file-hook 'my-disable-tramp-lockfiles)

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
      completions-max-height 40
      ;; make all completions case-insensitive
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(bind-key "C-n" 'minibuffer-next-completion minibuffer-mode-map)
(bind-key "C-p" 'minibuffer-previous-completion minibuffer-mode-map)

;; default value is too small
(setq gc-cons-threshold (* 4 1024 1024))
(setq read-process-output-max (* 1024 1024))

(setq undo-limit (* 8 1024 1024))

;; Delete trailing whitespaces before saving buffer
(add-hook 'before-save-hook 'whitespace-cleanup)

;; save minibuffer history between sessions
(savehist-mode t)

;; M-o is usually unbound, why waste such an easy keybinding?
(bind-key "M-o" #'other-window)

;; show column number in minibuffer
(column-number-mode t)

;; disable suspend-frame bindings as I sometimes hit them accidentally
(global-set-key (kbd "C-z") #'repeat)
(global-set-key (kbd "C-x C-z") nil)

(bind-key "M-#" #'rgrep)
(bind-key "M-*" #'bookmark-jump)

;; I use control for specifying prefix argument
;; so why not bind some useful things to easier keybindings
(dolist (bind-iter '(("1" "!")
             ("2" "@")
             ("3" "#")
             ("4" "$")
             ("5" "%")
             ("6" "^")
             ("7" "&")
             ("8" "*")
             ("9" "(")
             ("0" ")")))
  (define-key key-translation-map (kbd (concat "M-" (car bind-iter))) (kbd (concat "M-" (cadr bind-iter)))))

;; buffer-menu > buffer-list
(bind-key "C-x C-b" #'buffer-menu)
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)

;; do not confirm when killing buffer
(bind-key [remap kill-buffer] #'kill-this-buffer)

;; *-dwim is better than *-word
(bind-key "M-u" #'upcase-dwim)
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)

;; use f7 for reading documents
(bind-key "<f7>" #'scroll-lock-mode)

;; smooth scrolling (Emacs 29), NOTE: this doesn't work on commercial Emacs
(pixel-scroll-precision-mode t)

;; enhance isearch
(setq isearch-lazy-count t
      search-whitespace-regexp ".*?")

;; I love this commands
(bind-key "C-c -" #'align)
(bind-key "C-c 0" #'align-current)
(bind-key "C-c =" #'align-regexp)

(bind-key "C-(" #'insert-parentheses)

;; hl-line is very useful in some buffers, it helps reading
(bind-key "C-x x h" #'hl-line-mode)

;; Emacs 29: finally we don't need a separate package to duplicate lines
(bind-key "C-x d" #'duplicate-dwim)


;;; Utilities

;; abbrev mode
(require 'abbrev)
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

;; Give dired highlighting
(use-package diredfl
  :ensure t
  :hook (dired-mode  . diredfl-mode))

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
     ("C-c M-w" . avy-copy-line)
     ("C-c C-w" . avy-kill-whole-line)
     :map isearch-mode-map
     ("M-j" . avy-isearch)))

;; proced - Emacs process manager
;; https://laurencewarne.github.io/emacs/programming/2022/12/26/exploring-proced.html
(add-hook 'proced-mode-hook #'hl-line-mode)
;; this significantly slows Emacs if proced buffer stays open
;; (setq-default proced-auto-update-flag t)
(setq proced-goal-attribute nil
      proced-enable-color-flag t)
(setq-default proced-format 'long)

;; display ^L as horizontal lines
(use-package form-feed
  :ensure t
  :hook (emacs-lisp-mode . form-feed-mode))

;; setup eshell
(bind-key "C-:" #'project-eshell)

;; displays available keys if you forgot one of them
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; documentation
(global-eldoc-mode 1)

;; parentheses
(show-paren-mode t)
(setq show-paren-style 'mixed);; highlight brackets if visible, else entire expression
;; (electric-pair-mode t)
;; (electric-indent-mode nil)


;;; Programming modes

;; jump-to-defintion functionality for over 50 languages
(use-package dumb-jump
  :ensure t
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Greate article for setting Emacs for C/C++
;; https://tuhdo.github.io/c-ide.html

(setq-default c-basic-offset 4)  ;; Set indent width to 4 spaces
(setq-default tab-width 4)       ;; Set tab width to 4 spaces
(setq-default indent-tabs-mode nil)

;; improve syntax highlighting in C-based modes
;; https://emacs.stackexchange.com/questions/16750/better-syntax-higlighting-for-member-variables-and-function-calls-in-cpp-mode
(dolist (mode-iter '(c-mode c++-mode glsl-mode python-mode))
  ;; constants
  (font-lock-add-keywords
    mode-iter
    '(("\\<\[A-Z0-9_\]\+\\>" 0 'font-lock-constant-face keep)) t)
  ;; functions
  (font-lock-add-keywords
    mode-iter
    '(("\\([_a-zA-Z][_a-zA-Z0-9]*\\)\s*(" 1 'font-lock-function-name-face keep)) t))

(use-package company
  :ensure t
  :init
  (setq company-idle-delay nil
    company-require-match nil
    company-tooltip-minimum-width 60)
  :config (global-company-mode)
  :bind (("C-." . company-complete)))

(use-package glsl-mode
  :ensure t
  :mode (("\\.vert\\'" . glsl-mode)
     ("\\.frag\\'" . glsl-mode)
     ("\\.geom\\'" . glsl-mode)
     ("\\.comp\\'" . glsl-mode)
     ("\\.glsl\\'" . glsl-mode))
  :config
  (bind-key "C-<return>" 'c-next-line glsl-mode-map))

;; EIN for ipython notebooks
(use-package ein
  :ensure t
  :config
  (setq ein:use-auto-complete t)
  (setq ein:complete-on-dot t)
  (setq ein:completion-backend 'ein:use-company-backend)
  (setq ein:use-auto-complete-superpack nil)
  (setq ein:use-smartrep nil))

;; view pdf files
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (require 'dabbrev)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))


;;; UI

;; My custom modeline
(setq-default mode-line-format
      '("%e"
    " %*%@ "
    (:eval
     (propertize (format " %s" (buffer-name))
             'face 'mode-line-buffer-id))
    (vc-mode vc-mode)
    (:eval
     (list
      (propertize "  λ  " 'face 'shadow)
      (propertize (symbol-name major-mode) 'face 'bold)))))

;; theme
(load-theme 'tango-dark)

;; font
(set-frame-font "DejaVu Sans Mono 11" t t)

;; tabs
(tab-bar-mode)
;; unbind C-TAB, C-S-TAB
(global-set-key [(control tab)] nil)
(global-set-key [(control shift tab)] nil)
(global-set-key [(control shift iso-lefttab)] nil)
(bind-key "C-<right>" #'tab-next)
(bind-key "C-<left>" #'tab-previous)
;; change tab format
(defun lida/tab-bar-format (tab i)
  (propertize
   (alist-get 'name tab)
   'face (funcall tab-bar-tab-face-function tab)))
(setq tab-bar-tab-name-format-function #'lida/tab-bar-format)
;; move global data in modeline(such as time or battery status) to tab bar.
(setq lida/global-mode-string '("" display-time-string battery-mode-line-string))
(defun lida/tab-bar-format-global ()
  `((global menu-item ,(string-trim-right (format-mode-line lida/global-mode-string)) ignore)))
(setq tab-bar-format '(tab-bar-format-history
               tab-bar-format-tabs
               tab-bar-format-align-right
               lida/tab-bar-format-global))
(setq global-mode-string '(""))

;; https://www.emacswiki.org/emacs/WholeLineOrRegion
(defun lida/kill-ring-save (beg end flash)
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
(global-set-key [remap kill-ring-save] 'lida/kill-ring-save)
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))


;;; Misc

;; make Emacs keybindings work with russian keyboard
(use-package reverse-im
  :ensure t
  :config
  (setq reverse-im-input-methods '("russian-computer"))
  (reverse-im-mode t))

;; telegram
(use-package telega
  :ensure t
  :bind-keymap ("M-t" . telega-prefix-map)
  :config
  (setq telega-emoji-use-images nil)
  (setq telega-completing-read-function 'completing-read) ;; use builtin completion
  (require 'telega-mnz)
  (global-telega-mnz-mode t)
  (require 'telega-dired-dwim))

;; ;; matrix
;; (use-package ement
;;   :ensure t
;;   :bind ("<f2>" . ement-room-list))

;; Org mode
(setq org-catch-invisible-edits 'smart)

(defun lida/load-babel-languages ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((calc . t)
     (python . t)
     (C . t))))

;; syntax highlighting for code blocks in exported pdfs
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; the best mail and newsreader in da world
;; NOTE: `gnus-select-method' and `gnus-secondary-select-methods' are
;; set as customization variables. I set `gnus-select-method' to nnmaildir.
;; NOTE: fast searching with notmuch is set up with this:
;; (gnus-search-engine gnus-search-notmuch (remove-prefix "~/Mail"))
(use-package gnus
  :custom
  (gnus-asynchronous t)
  (gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (gnus-thread-hide-subtree t)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v\n")
  (gnus-use-cache t))

;; use smtp for sending
;; NOTE: password is stored in ~/.authinfo.gpg
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl)

(defun lida/update-maildir ()
  (interactive)
  (message "Updating maildir...")
  (start-process "mbsync" "*mbsync*" "mbsync" "-a"))

;; gnus-dired
(use-package gnus-dired
  :commands (gnus-dired-attach))

;; music in emacs
(require 'lida-music)



;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
