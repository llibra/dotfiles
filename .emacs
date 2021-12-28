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
              line-spacing 0
              indicate-empty-lines t)

;;;; UI

(when window-system
  ;; Font
  (let* ((family "M+ 1m")
         (size (if-windows "9" "14"))
         (style (if-windows "" ":weight=normal:slant=normal"))
         (base-font (concat family "-" size style)))
    (create-fontset-from-ascii-font base-font nil "mine")
    (let ((spec (font-spec :family family :weight 'normal :slant 'normal)))
      (set-fontset-font "fontset-mine" 'latin-jisx0201 spec)
      (set-fontset-font "fontset-mine" 'katakana-jisx0201 spec)
      (set-fontset-font "fontset-mine" 'japanese-jisx0208 spec)))

  ;; Frame
  (setq default-frame-alist
        `((width . 160)
          (height . 32)
          (font . "fontset-mine")
          ,@default-frame-alist))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode t))

(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

;;;; straight.el

(defvar bootstrap-version)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :straight t)

;;;; Theme

(use-package material-theme
  :config
  (load-theme 'material t)
  :straight t)

;;;; Shell

(require 'term)

(setq explicit-shell-file-name "zsh")

(when-windows
  (define-alternate-program "/bin/sh" '("fakecygpty" "/bin/sh")))

;; Kludge for term.el.
;; It refers not process-coding-system but locale-coding-system.
(add-hook 'term-mode-hook
          (lambda ()
            (make-local-variable locale-coding-system)
            (setq locale-coding-system 'utf-8)))

(when-windows
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m))

(when (package-installed-p 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;; Grep

;; For Cygwin. Prevent addition of NUL as a command line argument for grep.
(when-windows
  (add-hook 'grep-setup-hook
            (lambda ()
              (grep-apply-setting 'grep-use-null-device nil))))

;;;; SKK

(global-set-key (kbd "C-x C-j") 'skk-mode)

(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)

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

(use-package counsel
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

(require 'magit nil t)

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

;;; Caml Mode

(autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
(autoload 'camldebug "camldebug" "Run ocamldebug on program." t)

(when window-system
  (require 'caml-font nil t))

(setq inferior-caml-program (if-windows "sh -c ocaml" "ocaml")
      process-coding-system-alist
      (cons (if-windows '("^sh$" utf-8 . utf-8)
                        '("ocaml" utf-8 . utf-8))
            process-coding-system-alist)
      auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist)
      interpreter-mode-alist
      `(("ocamlrun" . caml-mode)
        ("ocaml" . caml-mode)
        ,@interpreter-mode-alist))

;; If "ocamlc -where" is called, OCaml built with Cygwin returns a Cygwin-style
;; path. Caml mode on non-Cygwin Emacs can't handle it.
(when-windows
  (setq ocaml-lib-path '("C:/Cygwin/usr/local/ocaml-4.00.0/lib/ocaml")))

;(add-hook 'caml-mode-hook 'viper-mode)

;;; Tuareg Mode

(load "tuareg-site-file" t)

(when (fboundp 'tuareg-mode)
  (setq tuareg-use-smie t
        tuareg-interactive-program inferior-caml-program
        tuareg-browser 'browse-url-firefox
        tuareg-library-path
        (if-windows "C:/Cygwin/usr/local/ocaml-4.00.0/lib/ocaml/"
                    "/usr/local/lib/ocaml/"))

  (add-hook 'tuareg-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command) "omake")))

  ;(add-hook 'tuareg-mode-hook 'viper-mode)
  )

;;; OCamlSpotter

(require 'ocamlspot nil t)

(when (featurep 'ocamlspot)
  (setq ocamlspot-command "ocamlspot.opt")

  (add-hook 'tuareg-mode-hook
            (lambda ()
              (define-key tuareg-mode-map "\C-c;" 'ocamlspot-query)
              (define-key tuareg-mode-map "\C-c:" 'ocamlspot-query-interface)
              (define-key tuareg-mode-map "\C-c'" 'ocamlspot-query-uses)
              (define-key tuareg-mode-map "\C-c\C-t" 'ocamlspot-type)
              (define-key tuareg-mode-map "\C-c\C-i" 'ocamlspot-xtype)
              (define-key tuareg-mode-map "\C-c\C-y" 'ocamlspot-type-and-copy)
              (define-key tuareg-mode-map "\C-cx" 'ocamlspot-expand)
              (define-key tuareg-mode-map "\C-c\C-u" 'ocamlspot-use)
              (define-key tuareg-mode-map "\C-ct" 'caml-types-show-type)
              (define-key tuareg-mode-map "\C-cp" 'ocamlspot-pop-jump-stack)))

  ;; Cygwin kludge

  (defun cygpath (args)
    (let* ((command (with-output-to-string
                      (princ "cygpath")
                      (mapc (lambda (x) (princ " ") (princ x)) args)))
           (result (shell-command-to-string command)))
      (replace-regexp-in-string "\n$" "" result)))

  (defun ocamlspot-query-string-at-cursor ()
    (let ((file-name (cygpath `(-u ,(prin1-to-string (buffer-file-name))))))
      (setq ad-return-value
            (format "%s:l%dc%d"
                    file-name
                    (ocamlspot-lines-of-point)
                    (ocamlspot-bytes-of-line-to-point)))))

  (defun ocamlspot-find-file-existing (path)
    (let ((path (cygpath `(-w ,path))))
      (if (file-exists-p path)
          (find-file-other-window path)
        (ocamlspot-message-add (format "ERROR: source file %s was not found" path))
        nil))))

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

;; To suppress error of list-packages.
(when (and (equal emacs-version "27.2") (eq system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; Custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
