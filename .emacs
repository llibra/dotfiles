;;;; .emacs - Initialization code of Emacs

;;;; Macros

(defmacro if-windows (then &optional else)
  `(if (eq window-system 'w32)
       ,then ,else))

(defmacro when-windows (&rest body)
  `(if-windows (progn ,@body)))
(put 'when-windows 'lisp-indent-function 0)

(defmacro unless-windows (&rest body)
  `(if-windows nil (progn ,@body)))
(put 'unless-windows 'lisp-indent-function 0)

(defmacro if-x (then &optional else)
  `(if (eq window-system 'x)
       ,then ,else))

(defmacro when-x (&rest body)
  `(if-x (progn ,@body)))
(put 'when-x 'lisp-indent-function 0)

(defmacro if-mac (then &optional else)
  `(if (eq window-system 'ns)
       ,then ,else))

(defmacro when-mac (&rest body)
  `(if-mac (progn ,@body)))
(put 'when-mac 'lisp-indent-function 0)

(defmacro if-wsl (then &optional else)
  `(if (getenv "WSL_INTEROP")
       ,then ,else))

(defmacro eval-after-load* (file &rest body)
  `(eval-after-load ,file '(progn ,@body)))
(put 'eval-after-load* 'lisp-indent-function 1)

;;;; Functions

(defun set-face-attributes (face frame attrs)
  (let* ((attrs (mapcar (lambda (x) (list (car x) (cdr x))) attrs))
         (attrs (apply 'nconc attrs)))
    (apply 'set-face-attribute face frame attrs)))

(defun true-color-p ()
  (>= (display-color-cells) 16777216))

(defun face-support/true-color-p ()
  (and (featurep 'faces) (true-color-p)))

(defconst tango-colors
  '((butter-1 . "fce94f")
    (butter-2 . "edd400")
    (butter-3 . "c4a000")
    (orange-1 . "fcaf3e")
    (orange-2 . "f57900")
    (orange-3 . "ce5c00")
    (chocolate-1 . "e9b96e")
    (chocolate-2 . "c17d11")
    (chocolate-3 . "8f5902")
    (chameleon-1 . "8ae234")
    (chameleon-2 . "73d216")
    (chameleon-3 . "4e9a06")
    (skyblue-1 . "729fcf")
    (skyblue-2 . "3465a4")
    (skyblue-3 . "204a87")
    (plum-1 . "ad7fa8")
    (plum-2 . "75507b")
    (plum-3 . "5c3566")
    (scarletred-1 . "ef2929")
    (scarletred-2 . "cc0000")
    (scarletred-3 . "a40000")
    (alminium-1 . "eeeeec")
    (alminium-2 . "d3d7cf")
    (alminium-3 . "babdb6")
    (alminium-4 . "888a85")
    (alminium-5 . "555753")
    (alminium-6 . "2e3436")))

(defun tango-color (name)
  (let ((entry (assq name tango-colors)))
    (unless entry (error "%s is unknown color name." name))
    (format "#%s" (cdr entry))))

;;;; Utilities

(defvar alternate-program-table
  (make-hash-table :test 'equal))

(defmacro define-alternate-program (program alt)
  `(puthash ,program ,alt alternate-program-table))

(defun alternate-program (program)
  (gethash program alternate-program-table))

(defadvice start-process (around kludge-for-hard-coding (name buffer program &rest program-args) activate)
  (let* ((alternate (alternate-program program))
         (program (or (if (consp alternate) (car alternate) alternate) program))
         (program-args (if (consp alternate) (append (cdr alternate) program-args) program-args)))
    ad-do-it))

;;;; Languages and encodings

(set-language-environment "Japanese")

(if-windows (progn
              (set-default-coding-systems 'cp932)
              (setq-default buffer-file-coding-system 'utf-8-unix))
            (set-default-coding-systems 'utf-8))
 
;;;; Variables

(setq inhibit-startup-screen t
      initial-scratch-message nil
      kill-whole-line t
      x-select-enable-clipboard t)

(setq-default indent-tabs-mode nil
              tab-width 8
              fill-colmun 80
              line-spacing 0.3
              indicate-empty-lines t)

;;;; UI

(when window-system
  ;; Font
  (set-face-attribute 'default nil :family "Maple Mono" :height 160)

  (let ((spec (font-spec :family "Kosugi Maru")))
    (set-fontset-font "fontset-default" 'latin-jisx0201 spec)
    (set-fontset-font "fontset-default" 'katakana-jisx0201 spec)
    (set-fontset-font "fontset-default" 'japanese-jisx0208 spec))

  ;; Frame
  (setq default-frame-alist
        `((fullscreen . maximized)
          ,@default-frame-alist))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode t))

(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;; Mode Line

(use-package nerd-icons :ensure t)

(use-package doom-modeline
  :after nerd-icons
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t)
  :custom-face
  ;; cf. https://github.com/seagle0128/doom-modeline/issues/632
  (doom-modeline-evil-normal-state ((t (:weight normal))))
  (doom-modeline-evil-insert-state ((t (:weight normal))))
  :ensure t)

;;;; Ansi Color

(eval-after-load* 'ansi-color
  (setq ansi-color-names-vector
        (vector "black"
                (tango-color 'scarletred-1)
                (tango-color 'chameleon-3)
                (tango-color 'butter-1)
                (tango-color 'skyblue-2)
                (tango-color 'plum-1)
                (tango-color 'skyblue-1)
                "white")
        ansi-color-map
        (ansi-color-make-color-map)))

;;;; Key bindings

(global-set-key "\C-h" 'delete-backward-char)

;;;; Awaking functions

;;; Region case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Narrowing
(put 'narrow-to-region 'disabled nil)

;;;; Abbrev

(setq save-abbrevs t)

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;;;; Font lock

(global-font-lock-mode t)

;;;; VIPER

;(setq viper-mode t)
;(require 'viper)

;;;; Dired

(require 'dired-x)

;; For suppression of overwriting the C-x C-j key binding. SKK uses it.
(setq dired-bind-jump nil)

;; h: Enable file size unit.
(setq dired-listing-switches "-alh")

(add-hook 'dired-mode-hook 'dired-omit-mode)

;;;; Diff

(setq diff-switches "-u")

;;;; Package System

(require 'package nil t)

(when (featurep 'package)
  (setq package-archives
        `(("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ,@package-archives))
  (package-initialize))

;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :ensure t)

;;;; Theme

(use-package apropospriate-theme
  :config
  (load-theme 'apropospriate-light t)
  :ensure t)

;;;; Shell

(use-package vterm :ensure t)

;;;; Grep

;; For Cygwin. Prevent addition of NUL as a command line argument for grep.
(when-windows
  (add-hook 'grep-setup-hook
            (lambda ()
              (grep-apply-setting 'grep-use-null-device nil))))

;;;; SKK

(use-package ddskk
  :bind*
  (("C-x C-j" . skk-mode))
  :custom
  (skk-status-indicator 'minor-mode)
  :hook
  (isearch-mode-hook . skk-isearch-mode-setup)
  (isearch-mode-end-hook . skk-isearch-mode-cleanup)
  :ensure t)

;;;; cygwin-mount.el

(require 'cygwin-mount nil t) 

(when (featurep 'cygwin-mount-autoloads)
  (cygwin-mount-activate))

;;;; Evil

(require 'evil nil t)

(when (featurep 'evil)
  ;; For prevent conflict of changing cursor color with SKK.
  (setq evil-default-cursor t)

  (global-undo-tree-mode)

  (custom-set-variables '(evil-undo-system 'undo-tree))
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  (evil-mode 1))

;;;; Shackle

(use-package shackle :ensure t
  :custom
  (shackle-rules '((magit-status-mode :align left)
                   (xref--xref-buffer-mode :align bottom)))

  :config
  (shackle-mode))

;;;; ace-window

(use-package ace-window :defer t :ensure t)

;;;; avy

(use-package avy :defer t :ensure t
  :custom
  (avy-timeout-seconds 1))

;;;; Ivy

(use-package counsel :ensure t
  :bind (("\C-s" . 'swiper)
         ("C-c C-r" . 'ivy-resume)
         ("<f6>" . 'ivy-resume)
         ("C-x C-f" . 'counsel-find-file)
         ("<f1> f" . 'counsel-describe-function)
         ("<f1> v" . 'counsel-describe-variable)
         ("<f1> l" . 'counsel-find-library)
         ("<f2> i" . 'counsel-info-lookup-symbol)
         ("<f2> u" . 'counsel-unicode-char)
         ("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c k" . 'counsel-ag)
         ("C-x l" . 'counsel-locate)
         ("C-S-o" . 'counsel-rhythmbox))
  :custom
  (counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  :config
  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-format-function 'ivy-format-function-default)

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (defun counsel-rg-default-directory (&optional initial-input initial-directory extra-rg-args rg-prompt)
    (interactive)
    (let ((extra-rg-args (concat "--no-ignore-vcs " extra-rg-args)))
      (counsel-rg initial-input default-directory extra-rg-args rg-prompt))))

;;;; Hydra

(use-package hydra :defer t :ensure t
  :bind
  ("M-x" . 'hydra-execute/body)
  :config
  (defhydra hydra-rg (:color blue :hint nil)
    "
_g_: Git Root   _c_: Default Directory
"
    ("g" counsel-rg)
    ("c" counsel-rg-default-directory)
    ("C-g" nil))

  (defhydra hydra-flymake (:color blue :hint nil)
    "
_n_: next error   _p_: previous error   _._: error at point
"
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("." display-local-help))

  (defhydra hydra-execute (:color blue :hint nil)
    "
_x_: execute command    _j_: jump to visible text   _w_: select window
_g_: full text search   _m_: magit                  _i_: info
_b_: bookmark           _d_: diagnostics

_C-g_: quit
"
    ("x" counsel-M-x)
    ("j" avy-goto-char-timer)
    ("w" ace-window)
    ("g" hydra-rg/body)
    ("m" magit-status)
    ("i" info)
    ("b" counsel-bookmark)
    ("d" hydra-flymake/body)
    ("C-g" nil)))

;;;; YASnippet

(require 'yasnippet nil t)

(when (featurep 'yasnippet)
  (yas-global-mode 1))

;;;; Git

(setq process-coding-system-alist
      (cons '("git" utf-8 . utf-8) process-coding-system-alist))

(use-package magit :ensure t)

;;;; Treemacs

(use-package treemacs
  :config
  (evil-set-initial-state 'treemacs-mode 'emacs))

;;;; C

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 2)))

;;;; Perl

(let ((perl5lib (getenv "PERL5LIB")))
  (setenv "PERL5LIB" (concat "~/perl5/lib/perl5" perl5lib))
  (setenv "PERL_CPANM_OPT" "--local-lib=~/perl5"))

;;;; PHP

(use-package php-mode :ensure t
  :hook (php-mode . php-enable-psr2-coding-style))

;;;; Ruby

(with-eval-after-load 'ruby-mode
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq ruby-insert-encoding-magic-comment nil))))

;;;; Emacs Lisp

;;;; Lisp

(setq inferior-lisp-program "sbcl")

(when (featurep 'slime-autoloads)
  (slime-setup '(slime-fancy))

  (add-hook 'lisp-mode-hook 'slime-mode)

  (setq slime-net-coding-system 'utf-8-unix
        slime-lisp-implementations `((sbcl ("sbcl"))
                                     (clisp ("clisp"))))

  (defun slime-space/skk (n)
    (interactive "p")
    (if (and (boundp 'skk-henkan-mode) skk-henkan-mode)
        (skk-insert n)
      (slime-space n)))

  (when (featurep 'skk-autoloads)
    (add-hook 'slime-mode-hook
              (lambda ()
                (define-key slime-mode-map " " 'slime-space/skk)))))

;;;; Scheme

(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(setq scheme-program-name "csi -:c"
      process-coding-system-alist
      `(("fakecygpty" utf-8 . utf-8)
        ("gosh" utf-8 . utf-8)
        ,@process-coding-system-alist))

(put 'receive 'scheme-indent-function 2)
(put 'let1 'scheme-indent-function 2)
(put 'and-let* 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)
(put 'with-error-to-port 'scheme-indent-function 0)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'define-method 'scheme-indent-function 2)

;(add-hook 'scheme-mode-hook 'viper-mode)

;;; Remote connection

(defvar run-remote-scheme-ssh "fakecygpty"
  "リモート接続に使うSSHコマンド")
(defvar run-remote-scheme-ssh-args "ssh -e none -t"
  "リモート接続に使うSSHコマンドに渡す引数")
(defvar run-remote-scheme-program scheme-program-name
  "リモートで実行するScheme処理系のコマンド")
(defvar run-remote-scheme-default-host "llibra@sigsig"
  "デフォルトで接続するリモートホスト")

(defun run-remote-scheme (host cmd)
  "Run remote scheme process. It doesn't look about not password-less authentication"
  (interactive 
   (if current-prefix-arg
       (list (read-string "Hostname: " run-remote-scheme-default-host)
             (read-string "Run Scheme: " run-remote-scheme-program))
     `(,run-remote-scheme-default-host ,run-remote-scheme-program)))
  (let ((scheme-program-name (format "%s %s %s %s"
                                     run-remote-scheme-ssh
                                     run-remote-scheme-ssh-args
                                     host
                                     cmd)))
    (run-scheme scheme-program-name)))

(defun run-remote-scheme-on-other-window (host cmd)
  "Run remote scheme process on other window"
  (interactive 
   (if current-prefix-arg
       (list (read-string "Hostname: " run-remote-scheme-default-host)
             (read-string "Run Scheme: " run-remote-scheme-program))
     `(,run-remote-scheme-default-host ,run-remote-scheme-program)))
  (save-excursion (save-window-excursion (run-remote-scheme host cmd)))
  (switch-to-buffer-other-window (get-buffer "*scheme*")))

(defun run-scheme-on-other-window ()
  "Run Scheme process on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

;;;; OCaml

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :ensure t)

(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode))
  :ensure t)

(use-package eglot :ensure t)

(use-package ocaml-eglot
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure)
  :ensure t)

;;;; Erlang

(setq erlang-root-dir "C:/Erlang/5.10.4")
(setq load-path (cons (format "%s/lib/tools-2.6.13/emacs" erlang-root-dir) load-path))

(require 'erlang-start nil t)

;;;; Scala

(require 'scala-mode-auto nil t)

;;;; Language Server Protocol

(use-package lsp-mode
  :ensure t
  :hook
  (js-mode . lsp)

  :init
  (add-hook 'php-mode-hook
            (lambda ()
              (setq lsp-file-watch-ignored-directories
                    `("[/\\\\]vendor\\'" "[/\\\\]storage\\'" ,@lsp-file-watch-ignored-directories))
              (lsp)))

  (evil-set-initial-state 'lsp-ui-imenu-mode 'emacs))

;;;; Debug Adapter Protocol

(use-package dap-mode
  :ensure t
  :config (dap-mode t)
          (dap-ui-mode t))

;;;; JavaScript

(with-eval-after-load 'js
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js-indent-level 2))))

;;;; web-mode.el

(when (package-installed-p 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-css-indent-offset 2)
              (setq web-mode-code-indent-offset 2))))

;;;; SQL

(autoload 'edbi:open-db-viewer "edbi" nil t)

(with-eval-after-load 'sql
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq truncate-lines t)))

  (if-windows
    (setq sql-mysql-options '("--local-infile" "-C" "-t" "-f" "-n"))
    (setq sql-mysql-options '("--local-infile")))

  (defun sql-get-product (name)
    (let ((connection (assoc 'food-oms sql-connection-alist)))
      (if connection
          (let ((variable (assoc 'sql-product connection)))
            (if variable
                (let ((value (nth 1 (cadr variable))))
                  (if value value sql-product))
              sql-product))
        sql-product)))

  (defun sql-connect* (connection)
    (interactive (list (sql-read-connection "Connection: " nil)))
    (let ((sql-product (sql-get-product connection)))
      (sql-connect connection connection))))

;;;; TRAMP

(require 'tramp)

(setq tramp-default-method "sshx")

;;;; Time Stamps

(require 'time-stamp)

(setq time-stamp-line-limit 10
      time-stamp-start "\\Last modified:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "%04y-%02m-%02dT%02H:%02M:%02S+09:00")

(add-hook 'write-file-hooks 'time-stamp)

;;;; nXML

(setq nxml-default-buffer-file-coding-system 'utf-8
      nxml-auto-insert-xml-declaration-flag t
      nxml-slash-auto-complete-flag t
      auto-mode-alist
      `(("\\.xml$" . nxml-mode)
        ("\\.xsl$" . nxml-mode)
        ,@auto-mode-alist))

;(add-hook 'nxml-mode-hook 'viper-mode)

;;;; Emacsclient

(server-start)

;;;; Navi2ch

(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

(setq navi2ch-article-auto-range nil
      navi2ch-article-auto-expunge t
      browse-url-generic-program "google-chrome"
      browse-url-browser-function 'browse-url-generic)

;;;; Misc

(cd "~")

(load "~/.emacs.d/site.el" t)

(require 'cl)

(define-derived-mode blogger-form-mode nxml-mode "Blogger-Form"  
  "Major mode for edit Blogger's post form"
  (setq buffer-file-coding-system 'utf-8)
  (save-excursion
    (goto-char (point-min))
    (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
            "  <head>\n"
            "    <title>blogger-form-mode</title>\n"
            "  </head>\n"
            "  <body>\n"
            "    <div>\n"
            "<!-- content -->\n")
    (let ((content-min (point))
          (content-max (point-max)))
      (goto-char (point-max))
      (insert "<!-- /content -->\n"
              "    </div>\n"
              "  </body>\n"
              "</html>")
      (narrow-to-region content-min content-max)))
  (rng-auto-set-schema-and-validate)
  (set-buffer-modified-p nil)
  (defun blogger-form-save-buffer ()
    (interactive)
    ;; 呼ばれる状況が限定されるので、色々と省略
    (write-region (point-min) (point-max) (buffer-file-name))
    (set-visited-file-modtime)
    (set-buffer-modified-p nil))
  (defun blogger-form-server-edit ()
    (interactive)
    (blogger-form-save-buffer)
    (server-edit))
  (define-key blogger-form-mode-map "\C-x\C-s" 'blogger-form-save-buffer)
  (define-key blogger-form-mode-map "\C-x#" 'blogger-form-server-edit))

;(setq auto-mode-alist
;      (acons (format "^%s/ViewSourceWith/www[0-9]*\\.blogger\\.com_post"
;                     (w32-long-file-name (getenv "TEMP")))
;             'blogger-form-mode
;             auto-mode-alist))

(defun refer-gauche-reference (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((substring (buffer-substring (point-min) (point-max))))
        (delete-region (point-min) (point-max))
        (insert
         (format (concat "<a href=\"http://practical-scheme.net/gauche/man/"
                         "?l=jp&amp;p=%s\">%s</a>")
                 (url-hexify-string substring)
                 (xml-escape-string substring))))
      )))

(defun xml-escape-string (x)
  (reduce #'(lambda (s e)
              (replace-regexp-in-string (car e) (cdr e) s))
          '(("<" . "&lt;")
            (">" . "&gt;")
            ("&" . "&amp;"))
          :initial-value x))

;;; Work Around

;;; Custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
