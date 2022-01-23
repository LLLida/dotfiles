;; -*- lexical-binding: t; -*-

(use-package god-mode
  :init
  (setq god-mode-enable-function-key-translation nil
        god-exempt-major-modes '(Info-mode calculator-mode calendar-mode
                                           compilation-mode debugger-mode
                                           dired-mode ediff-mode eww-mode
                                           git-commit-mode grep-mode ibuffer-mode
                                           magit-popup-mode org-agenda-mode pdf-outline-mode)
        god-exempt-predicates nil)
  :bind
  (("C-x C-1" . delete-other-windows)
   ("C-x C-2" . split-window-below)
   ("C-x C-3" . split-window-right)
   ("C-x C-0" . delete-window)
   ("C-x C-o" . other-window)
   ("<escape>" . god-mode-all)
   :map god-local-mode-map
   ("[" . backward-paragraph)
   ("]" . forward-paragraph)
   ("z" . switch-to-buffer)
   ("." . repeat)
   ("i" . god-mode-all)
   ("u" . undo)
   ("q" . kill-buffer)
   ("m" . xah-goto-matching-bracket))
  :diminish god-local-mode
  :config
  (require 'god-mode-isearch)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable))

(defvar my/insert-mode-line-number-color "#00ff7f")
(defvar my/god-mode-line-number-color "#ff7f50")

(defun my/enable-god-mode ()
  (when (featurep 'smart-mode-line)
    (set-face-attribute 'sml/line-number nil
                        :foreground my/god-mode-line-number-color))
  (setq cursor-type 'box))
(defun my/disable-god-mode ()
  (when (featurep 'smart-mode-line)
    (set-face-attribute 'sml/line-number nil
                        :foreground my/insert-mode-line-number-color))
  (setq cursor-type 'bar))
(add-hook 'god-mode-enabled-hook #'my/enable-god-mode)
(add-hook 'god-mode-disabled-hook #'my/disable-god-mode)

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
        (goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'my/kill-ring-save)
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(defvar xah-left-brackets '( "(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "〘")
  "List of left bracket chars.")
(defvar xah-right-brackets '( ")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "〙")
  "list of right bracket chars.")
(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))
(global-set-key (kbd "C-m") 'xah-goto-matching-bracket)

(bind-key "<left>" 'isearch-repeat-backward isearch-mode-map)
(bind-key "<right>" 'isearch-repeat-forward isearch-mode-map)
(bind-key "C-'" 'backward-delete-char-untabify)

(provide 'lida-keys)
