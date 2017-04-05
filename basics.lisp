;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Implementation-independent basics
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conventions
;;; - We follow LispWorks general design priciples.
;;; - We prefer Allegro's :null-terminate key parameter name to LW's :null-terminated-p.
;;;
;;; LispWorks and SBCL Notes
;;; - When NIL is passed to a foreign routine, the NULL pointer is substituted
;;;   instead of it automatically.

(in-package :ystok.ffc)

;; When a file invokes define-... macros and does not pass the :module argument
;; explicitly, this must be set at compile (or load?) time beforehand.
(defvar *default-foreign-module* nil)

(defgeneric module-real-name (module)
 ;;; Values 1) Patnname or string that is to be passed to an implementation-specific
  ;;           link/load/register-module function.
  ;;        2? lifetime ?
  ;;        3? connection-style ?
  ;; Specialized methods can return an explicit pathname.
 (:method (name)
  (let* ((string #+win32 (string-upcase name)
                 #-win32 (string name))
         (length (length string)))
    (if (and (plusp length)
             (or (char= (char string (1- length)) #\.)		; last char is '.'
                 (string-equal string #1= #+win32  ".DLL"
                                          #+darwin ".dylib"
                                          #+linux  ".so"
                               :start1 (max 0 (- length (length #1#))))))
        string
        (concatenate 'string string #1#))))
 (:method ((module pathname))
  module) )

(pushnew :ystok-ffc *features*)
