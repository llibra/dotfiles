;;;; .emacs - Initialization code of Emacs

;;;; Macros

(defmacro if-windows (then &optional else)
  `(if (eq window-system 'w32)
       ,then ,else))

(defmacro when-windows (&rest body)
  `(if-windows (progn ,@body)))

(defmacro unless-windows (&rest body)
  `(if-windows nil (progn ,@body)))

(defmacro if-x (then &optional else)
  `(if (eq window-system 'x)
       ,then ,else))

(defmacro when-x (&rest body)
  `(if-x (progn ,@body)))

;;;; Languages and encodings

(set-language-environment "Japanese")

(set-default-coding-systems 'utf-8-unix)
(set-clipboard-coding-system (if-windows 'sjis-dos 'utf-8-unix))

(when-windows
  (set-w32-system-coding-system 'sjis-dos))

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
  (let ((base-font (if-windows "M+ 1m-9" "Meguri-10")))
    (create-fontset-from-ascii-font base-font nil "mine"))

  (when-windows
    (mapc (lambda (target)
            (set-fontset-font "fontset-mine"
                              target
                              (font-spec :family "Meguri")
                              nil
                              'append))
          '(katakana-jisx0201 japanese-jisx0213.2004-1 japanese-jisx0213-2)))

  ;; General Punctuation
  (set-fontset-font "fontset-mine" '(#x2000 . #x206f) "M+ 1m")

  ;; Frame
  (setq default-frame-alist
        `((width . ,(if-x 60 120))
          (height . 36)
          (font . "fontset-mine")
          ,@default-frame-alist))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode t))

(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)

;;;; Faces

(when window-system
  (set-face-attribute 'default nil
                      :foreground "#1a1a1a"
                      :background "#ffffff")
  (set-face-attribute 'cursor nil
                      :background "#666666")
  (set-face-attribute 'mode-line nil
                      :box "#ffffff"
                      :foreground "#ffffff"
                      :background "#333333")
  (set-face-attribute 'mode-line-buffer-id nil
                      :weight 'normal)
  (set-face-attribute 'mode-line-highlight nil
                      :box "#ffffff"
                      :background "#999999")
  (set-face-attribute 'mode-line-inactive nil
                      :box "#ffffff"
                      :foreground "#ffffff"
                      :background "#aaaaaa")
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground "#3465a4")
  (set-face-attribute 'fringe nil
                      :foreground "#ffffff"
                      :background "#dddddd")
  (set-face-attribute 'header-line nil
                      :box "#ffffff"
                      :foreground "#ffffff"
                      :background "#666666")
  (set-face-attribute 'highlight nil
                      :underline t
                      :foreground "#ef2929"
                      :background "#ffffff")
  (set-face-attribute 'region nil
                      :foreground "#ffffff"
                      :background "#aaaaaa")
  (set-face-attribute 'isearch nil
                      :weight 'normal
                      :underline t
                      :foreground "#ef2929"
                      :background 'unspecified)
  (set-face-attribute 'match nil
                      :foreground "#ef2929"
                      :background 'unspecified
                      :underline t))

;;;; Key bindings

(global-set-key "\C-h" 'delete-backward-char)

;;;; Awaking functions

;;; Region case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; Narrowing
(put 'narrow-to-region 'disabled nil)

;;;; Abbrev

(setq abbrev-file-name "~/.abbrev_defs"
      save-abbrevs t)
(quietly-read-abbrev-file)

;;;; Font lock

(global-font-lock-mode t)

(when window-system
  (add-hook 'font-lock-mode-hook
            (lambda ()
              (set-face-attribute 'font-lock-builtin-face nil
                                  :weight 'normal
                                  :foreground "#c4a000")
              (set-face-attribute 'font-lock-keyword-face nil
                                  :weight 'normal
                                  :foreground "#3465a4")
              (set-face-attribute 'font-lock-function-name-face nil
                                  :foreground "#4e9a06")
              (set-face-attribute 'font-lock-variable-name-face nil
                                  :foreground "#06989a")
              (set-face-attribute 'font-lock-constant-face nil
                                  :foreground "#9999aa")
              (set-face-attribute 'font-lock-string-face nil
                                  :foreground "#9999aa")
              (set-face-attribute 'font-lock-comment-face nil
                                  :foreground "#9999aa")
              (set-face-attribute 'font-lock-warning-face nil
                                  :weight 'normal))))

;;;; Show Paren

(show-paren-mode t)

(when window-system
  (set-face-attribute 'show-paren-match nil
                      :underline "#3465a4"
                      :foreground 'unspecified
                      :background 'unspecified)
  (set-face-attribute 'show-paren-mismatch nil
                      :underline "#ef2929"
                      :foreground 'unspecified
                      :background 'unspecified))

;;;; VIPER

(setq viper-mode t)
(require 'viper)

;;;; Info

(when window-system
  (add-hook 'Info-mode-hook
            (lambda ()
              (set-face-attribute 'info-header-xref nil
                                  :foreground "#729fcf")
              (set-face-attribute 'info-xref nil
                                  :underline t
                                  :foreground "#3465a4")
              (set-face-attribute 'info-xref-visited nil
                                  :foreground "#75507b")
              (set-face-attribute 'info-node nil
                                  :slant 'normal
                                  :foreground "#3465a4"))))

;;;; Shell

(require 'term)

(setq explicit-shell-file-name "zsh")

(when-windows
  ;; Kludge. term-exec-1 in lisp/term.el fails on Windows due to hard coding.
  (defun term-exec-1 (name buffer command switches)
    ;; We need to do an extra (fork-less) exec to run stty.
    ;; (This would not be needed if we had suitable Emacs primitives.)
    ;; The 'if ...; then shift; fi' hack is because Bourne shell
    ;; loses one arg when called with -c, and newer shells (bash,  ksh) don't.
    ;; Thus we add an extra dummy argument "..", and then remove it.
    (let ((process-environment
           (nconc
            (list
             (format "TERM=%s" term-term-name)
             (format "TERMINFO=%s" data-directory)
             (format term-termcap-format "TERMCAP="
                     term-term-name term-height term-width)
             ;; We are going to get rid of the binding for EMACS,
             ;; probably in Emacs 23, because it breaks
             ;; `./configure' of some packages that expect it to
             ;; say where to find EMACS.
             (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)
             (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
             (format "LINES=%d" term-height)
             (format "COLUMNS=%d" term-width))
            process-environment))
          (process-connection-type t)
          ;; We should suppress conversion of end-of-line format.
          (inhibit-eol-conversion t)
          ;; The process's output contains not just chars but also binary
          ;; escape codes, so we need to see the raw output.  We will have to
          ;; do the decoding by hand on the parts that are made of chars.
          (coding-system-for-read 'binary))
      (apply 'start-process name buffer
             "f_sh" "-c"
             (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
                     term-height term-width)
             ".."
             command switches))))

(when-windows
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m))

(when window-system
  (add-hook 'shell-mode-hook
            (lambda ()
              (set-face-attribute 'comint-highlight-prompt nil
                                  :weight 'normal
                                  :foreground "#1144bb"))))

;;;; Compilation

(when window-system
  (add-hook 'compilation-mode-hook
            (lambda ()
              (set-face-attribute 'compilation-info nil
                                  :foreground "#4e9a06"
                                  :weight 'normal)
              (set-face-attribute 'compilation-error nil
                                  :foreground "#ef2929")
              (set-face-attribute 'compilation-line-number nil
                                  :foreground "#06989a"))))

;;;; Grep

;; For Cygwin. Prevent addition of NUL as a command line argument for grep.
(when-windows
  (add-hook 'grep-setup-hook
            (lambda ()
              (grep-apply-setting 'grep-use-null-device nil))))

;;;; Auto Complete Mode

(require 'auto-complete-config nil t)

(when (featurep 'auto-complete)
  (ac-config-default))

;;;; YASnippet

(require 'yasnippet nil t)

(when (featurep 'yasnippet)
  (yas-global-mode 1))

;;;; C

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 2)))

;;;; Lisp

(if-windows (setq inferior-lisp-program "sbcl")
            (setq inferior-lisp-program "ccl"))

(add-hook 'lisp-mode-hook 'viper-mode)

(load (expand-file-name "~/quicklisp/slime-helper.el") t)

(when (featurep 'slime-autoloads)
  (add-hook 'lisp-mode-hook 'slime-mode)

  (setq slime-net-coding-system 'utf-8-unix
        slime-lisp-implementations `((ccl ("wx86cl"))
                                     (sbcl ("sbcl"))
                                     (clisp ("clisp"))))
  (add-hook 'slime-connected-hook
            (lambda ()
              (slime-repl-set-package ":llibra")))

  (when window-system
    (add-hook 'slime-mode-hook
              (lambda () 
                (set-face-attribute 'slime-repl-prompt-face nil
                                    :foreground "#3465a4")
                (set-face-attribute 'slime-error-face nil
                                    :underline "#ef2929")
                (set-face-attribute 'slime-warning-face nil
                                    :underline "#c4a000")
                (set-face-attribute 'slime-inspector-action-face nil
                                    :foreground "#ef2929")
                (set-face-attribute 'slime-repl-inputed-output-face nil
                                    :foreground "#ef2929")
                (set-face-attribute 'slime-repl-output-mouseover-face nil
                                    :box nil
                                    :underline t))))

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

(add-hook 'scheme-mode-hook 'viper-mode)

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

(add-hook 'caml-mode-hook 'viper-mode)

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

  (when window-system
    (add-hook 'tuareg-mode-hook
              (lambda ()
                (set-face-attribute 'tuareg-font-lock-governing-face nil
                                    :foreground (face-attribute 'font-lock-keyword-face :foreground)
                                    :weight 'normal)
                (set-face-attribute 'tuareg-font-lock-operator-face nil
                                    :foreground "#999999"))))

  (add-hook 'tuareg-mode-hook 'viper-mode))

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

  (when window-system
    (add-hook 'tuareg-mode-hook
              (lambda ()
                (set-face-attribute 'ocamlspot-spot-face nil
                                    :foreground "#ef2929"
                                    :background 'default
                                    :underline t)
                (set-face-attribute 'ocamlspot-tree-face nil
                                    :foreground "#3465a4"
                                    :background 'default
                                    :underline t))))

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

;;;; Java

(require 'ajc-java-complete-config nil t)

(when (featurep 'ajc-java-complete)
  (add-hook 'java-mode-hook 'ajc-java-complete-mode))

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

(add-hook 'nxml-mode-hook 'viper-mode)

;;;; Emacsclient

(server-start)

;;;; Navi2ch

(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

(setq navi2ch-article-auto-range nil
      navi2ch-article-auto-expunge t
      browse-url-generic-program "google-chrome"
      browse-url-browser-function 'browse-url-generic)

(when window-system
  (add-hook 'navi2ch-face-load-hook
            (lambda ()
              (set-face-attribute 'navi2ch-list-category-face nil
                                  :weight 'normal
                                  :foreground "#c4a000")
              (set-face-attribute 'navi2ch-list-board-name-face nil
                                  :foreground "#06989a")
              (set-face-attribute 'navi2ch-article-header-face nil
                                  :weight 'normal
                                  :foreground "#4e9a06")
              (set-face-attribute 'navi2ch-article-header-contents-face nil
                                  :foreground "#999999")
              (set-face-attribute 'navi2ch-article-header-fusianasan-face nil
                                  :foreground "#3465a4")
              (set-face-attribute 'navi2ch-article-link-face nil
                                  :weight 'normal
                                  :underline t
                                  :foreground "#3465a4")
              (set-face-attribute 'navi2ch-article-url-face nil
                                  :weight 'normal
                                  :underline t
                                  :foreground "#3465a4")
              (set-face-attribute 'navi2ch-article-message-separator-face nil
                                  :foreground "#999999")
              (set-face-attribute 'navi2ch-bm-cache-face nil
                                  :foreground "#75507b")
              (set-face-attribute 'navi2ch-bm-view-face nil
                                  :foreground "#ef2929")
              (set-face-attribute 'navi2ch-bm-update-face nil
                                  :foreground "#3465a4")
              (set-face-attribute 'navi2ch-bm-mark-face nil
                                  :underline nil
                                  :foreground "#4e9a06"))))

;;;; Misc

(cd "~")

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
