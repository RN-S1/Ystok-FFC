;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - ASDlite/ASDF system definition
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.

(in-package :cl-user)

(asdf:defsystem ystok-ffc
  :version "0.1.001"
  :description "Foreign Function Compatibility layer"
  :maintainer "Dmitry Ivanov http://lisp.ystok.ru/"
  :licence "LLGPL"
  #+asdlite #+asdlite
  :output-pathname (asd:current-location "bin/")
  :serial t
  :components
  ((:file "package")
   (:file "basics")
   #+lispworks (:file "lispworks")
   #+sbcl      (:file "sbcl")
   (:file "shared")
   #+win32 (:file "win32-ffi")
   #+win32 (:file "win32-shared")))

;(load #P"PROJECTS:ylib;ffc:ystok-ffc.asd")
;(asd:operate :load 'ystok-ffc)
;(asdf:operate 'asdf:load-op 'ystok-ffc)
