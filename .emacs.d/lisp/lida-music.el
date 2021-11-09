;; Adil Mokhammad's config for music in GNU/Emacs

;; I use bongo(code mostly is taken from Protesilaos Stavrou)
;; https://protesilaos.com/codelog/2020-08-06-emacs-bongo-extras/
(use-package bongo
  :config
  (setq bongo-default-directory "~/Music"
        bongo-insert-whole-directory-trees t
        bongo-display-track-icons nil
        bongo-display-track-lengths nil
        bongo-display-header-icons t
        bongo-display-playback-mode-indicator t
        bongo-display-inline-playback-progress t
        bongo-join-inserted-tracks nil
        bongo-field-separator (propertize " · " 'face 'shadow)
        bongo-header-line-mode t
        bongo-mode-line-indicator-mode nil
        bongo-enabled-backends '(vlc mpv))
  :bind (("<C-XF86AudioPlay>" . bongo-pause/resume)
         ("<C-XF86AudioNext>" . bongo-next)
         ("<C-XF86AudioPrev>" . bongo-previous)
         ("<M-XF86AudioPlay>" . bongo-show)
         ("<S-XF86AudioNext>" . bongo-seek-forward-10)
         ("<S-XF86AudioPrev>" . bongo-seek-backward-10)
         :map bongo-playlist-mode-map
         ("n" . bongo-next-object)
         ("p" . bongo-previous-object)
         ("r" . prot/bongo-playlist-random-toggle)
         ("R" . bongo-rename-line)
         :map xah-fly-command-map
         ("SPC SPC" . bongo-pause/resume)
         ("SPC <right>" . bongo-skip-current)
         ("SPC <left>" . bongo-previous)))

(defun prot/bongo-playlist-play-random ()
  "Play random `bongo' track and determine further conditions."
  (interactive)
  (unless (bongo-playlist-buffer)
    (bongo-playlist-buffer))
  (when (or (bongo-playlist-buffer-p)
            (bongo-library-buffer-p))
    (unless (bongo-playing-p)
      (with-current-buffer (bongo-playlist-buffer)
        (bongo-play-random)
        (bongo-random-playback-mode 1)
        (bongo-recenter)))))

(defun prot/bongo-playlist-random-toggle ()
  "Toggle `bongo-random-playback-mode' in playlist buffers."
  (interactive)
  (if (eq bongo-next-action 'bongo-play-random-or-stop)
      (bongo-progressive-playback-mode)
    (bongo-random-playback-mode)))

(provide 'lida-music)
;; lida-music.el ends here
