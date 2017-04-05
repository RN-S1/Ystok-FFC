;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Package definition
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :ystok.ffc
 (:nicknames :ffc)
 (:use :common-lisp) ;:ystok.library)

 #+(and lispworks (not lispworks4.3))
 (:import-from :fli
  fli:*null-pointer*)

 #+(and lispworks win32)
 (:import-from :win32
  win32:WORD win32:DWORD)

 #+(and sbcl win32)
 (:import-from :sb-win32
  sb-win32:DWORD)

 (:export			; YSQL/FFC name (obsolete)
  ;; Libraries
  #:*default-foreign-module*
  #:load-foreign-module
  #:module-loaded-pathname
  #:module-real-name
  #:unload-foreign-module

  ;; Low-level functions and macros
  #:*null-pointer*
  #:%alloc			;%new-ptr (was a function), %make-struct
  #:%alloc-vector		;%new-vector
  #:%copy-pointer
  #:%decf-pointer		;%decf-ptr
  #:%deref			;%get-ptr
  ;#:%fill-vector
  #:%free			;%dispose-ptr
  #:%incf-pointer		;%incf-ptr
  #:%length
  #:%null-pointer-p		;%null-ptr-p
  #:%pointer-address		;%pointer-to-address
  #:%pointer-eql		;%ptr-eql
  #:%pointer-element-size
  #:%pointerp
  #:%size-of
  #:%slot			;%foreign-slot
  #:%slot-pointer		;%foreign-slot-pointer
  #:%svref
  #:%svref-pointer

  ;; Elementary types, readers, and writers
  #:WORD #:DWORD
  #:%get-octet  #:%put-octet
  #:%get-word   #:%put-word
  #:%get-dword  #:%put-dword
  #:%get-binary #:%put-binary

  ;; Strings
  #:*foreign-char-type*
  #:*foreign-external-format*
  #:%get-char #:%put-char
  #:%cstring-lisp		;%get-cstring
  #:%lisp-cstring
  #:%multi-sz-list
  #:%null-terminate

  ;; High-level macros
  #:define-foreign-function
  #:define-foreign-struct
  #:define-foreign-type
  #:define-foreign-variable
  #:with-cstring		;with-cstr (element-count/length, byte-count/octet-length)
  ;#:with-cstrings
  ;#:without-scheduling
  #:with-foreign		;with-temporary-allocation
  #:with-pointer		;with-dynamic-pointer
  . 
  #-win32 ()
  #+win32
  (#:*default-win32api-module*
   #:define-win32api
   #:get-locale-info
   #:%wstring-upper-down-case
)))

#|
 #+unused #:%address-pointer		;%address-to-pointer
 #+unused #:%length

 ;; Other from YSQL
  ;#:*foreign-ef*
  ;#+win32 #:*foreign-code-page*
  ;#:initialize-ffc-locale

  #:+sql-null-handle+
  ;; SQL types
  #:sql-byte
  #:sql-char
  #:sql-char-ptr
  #:sql-wchar
  #:sql-double
  #:sql-handle" 
  #:sql-handle-ptr
  #:sql-integer
  #:sql-octet
  #:sql-pointer
  #:sql-real
  #:sql-return
  #:sql-smallint
  #:sql-schar
  #:sql-uinteger
  #:sql-usmallint
  #:string-ptr" 
  ;"%with-sql-pointer" 
  ;; Buffer to/from lisp converting
  #:%buffer-into-vector
  #:%get-char
  #:%get-string  #:%put-string
  #:%get-wstring #:%put-wstring
  #:%new-cstring" 

  #:%get-integer   ;#:%put-integer
  #:%get-smallint  ;#:%put-smallint
  #:%get-usmallint ;#:%put-uinteger
  #:%get-uinteger  ;#:%put-usmallint

  #:%get-slong  #:%put-slong
  #:%get-ulong  #:%put-ulong
  #:%get-sshort #:%put-sshort
  #:%get-ushort #:%put-ushort
  #:%get-single #:%put-single
  #:%get-double #:%put-double
  #:%get-bit    #:%put-bit
  #:%get-byte   #:%put-byte

  #:%new-binary
  #:%ensure-hwnd
|#
