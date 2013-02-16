;;;; .ccl-init.lisp - Clozure CL initialization file

(in-package :cl-user)

;;; External Formats

(setf ccl:*default-external-format* :utf-8)
(setf ccl:*default-file-character-encoding* :utf-8)
(setf ccl:*default-socket-character-encoding* :utf-8)

;;; ASDF

(require :asdf)

;;; Quicklisp

#-windows
(load "home:share;lisp;quicklisp;setup")
#+windows
(load "home:quicklisp;setup")

;;; Working Package Definition

(ql:quickload "iterate")
(ql:quickload "cl-fn")
(ql:quickload "optima")

(defpackage :llibra (:use :cl :iterate :cl-fn :optima))
