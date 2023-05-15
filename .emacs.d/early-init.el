;; Disable some UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-language-environment 'UTF-8) ;; UTF-8 everywhere
(setq auto-window-vscroll nil
      x-wait-for-event-timeout nil
      split-width-threshold 9999
      ring-bell-function 'ignore
      )

(setq package-enable-at-startup nil)
