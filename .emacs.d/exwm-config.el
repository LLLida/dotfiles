;; My exwm config

;;; Create a file 'emacs.desktop' in /usr/share/xsessions/ with following contents:
;; [Desktop Entry]
;; Encoding=UTF-8
;; Name=GNU/Emacs
;; Comment=EXWM
;; Exec=emacs -mm --debug-init --load ~/.emacs.d/exwm-config.el
;; Type=XSession

(require 'exwm)
(require 'exwm-systemtray)

;; Set the initial workspace number.
(setq exwm-workspace-number 4)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; (setq exwm-systemtray-height 32)
(exwm-systemtray-enable)

;; Global keybindings.
(setq exwm-input-global-keys
      `(
        ;; 's-r': Reset (to line-mode).
        ([?\s-r] . exwm-reset)
        ;; 's-w': Switch workspace.
        ([?\s-w] . exwm-workspace-switch)
        ;; 's-d': Launch application. I made s-d so it is the same as dmenu
        ([?\s-d] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; 's-N': Switch to certain workspace.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))

(defun xbacklight-increase ()
  (interactive)
  (start-process-shell-command "xbacklight + 10" nil "xbacklight + 10"))
(defun xbacklight-decrease ()
  (interactive)
  (start-process-shell-command "xbacklight - 10" nil "xbacklight - 10"))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'xbacklight-increase)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'xbacklight-decrease)

(defun sound-increase ()
  (interactive)
  (shell-command-to-string "amixer set Master 10%+"))

(defun sound-decrease ()
  (interactive)
  (shell-command-to-string "amixer set Master 10%-"))

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'sound-increase)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'sound-decrease)

(defun screenshot-whole (&optional delay)
  (interactive "P")
  (let ((default-directory (expand-file-name "~/Pictures/Screenshots/"))
        (command (if (and delay
                          (numberp delay)
                          (> delay 0))
                     (format "scrot --delay %d" delay)
                   "scrot")))
    (start-process-shell-command "screenshot-partial" nil command)))

(defun screenshot-partial (&optional delay)
  (interactive "P")
  (let ((default-directory (expand-file-name "~/Pictures/Screenshots/"))
        (command (if (and delay
                          (numberp delay)
                          (> delay 0))
                     (format "scrot -s --delay %d" delay)
                   "scrot -s")))
    (start-process-shell-command "screenshot-partial" nil command)))

(defun screenshot-current-window ()
  (interactive)
  (message "saved screenshot to %s"
   (shell-command-to-string "scrot -u")))

(exwm-input-set-key (kbd "<print>") 'screenshot-whole)
(exwm-input-set-key (kbd "C-<print>") 'screenshot-partial)
(exwm-input-set-key (kbd "s-<print>") 'screenshot-window)

;; Line-editing shortcuts
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\C-v] . [next])
        ([?\M-v] . [prior])
        ([?\M-w] . [C-c])
        ([?\C-y] . [C-v])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))

;; Enable EXWM
(exwm-enable)

;; nm-applet
;; blueman-applet
;; (telega-appindicator-mode 1)
