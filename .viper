;;;; -*-Emacs-Lisp-*-
;;;; .viper - Viper-specific customization file

;;; Basic

(setq viper-inhibit-startup-message t
      viper-expert-level 5)

;;; Mode Association

(setq viper-vi-state-mode-list
      `(scheme-mode
        lisp-mode
        emacs-lisp-mode
        caml-mode
        java-mode
        tuareg-mode
        nxml-mode
        ,@viper-vi-state-mode-list))

(setq viper-emacs-state-mode-list
      `(inferior-scheme-mode
        inferior-caml-mode
        tuareg-interactive-mode
        ,@viper-emacs-state-mode-list))

;;; UI

(when (viper-has-face-support-p)
  (setq viper-insert-state-cursor-color nil)
  (setq viper-replace-overlay-cursor-color nil)
  (set-face-attribute 'viper-replace-overlay nil
                      :underline "#ef2929"
                      :foreground 'unspecified
                      :background 'unspecified)
  (set-face-attribute 'viper-search nil
                      :underline "#ef2929"
                      :foreground 'unspecified
                      :background 'unspecified)
  (set-face-attribute 'viper-minibuffer-vi nil
                      :foreground 'unspecified
                      :background 'unspecified)
  (set-face-attribute 'viper-minibuffer-insert nil
                      :foreground 'unspecified
                      :background 'unspecified)
  (set-face-attribute 'viper-minibuffer-emacs nil
                      :foreground 'unspecified
                      :background 'unspecified))
