;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Lispworks system definition
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;(pushnew :debug *features*)
;(lw:removef *features* :debug)
;(hcl:toggle-source-debugging #+debug t #-debug nil)

(defsystem YSTOK-FFC (:object-pathname (lw:current-pathname #-debug "bin/"
                                                            #+debug "bin/debug/"))
 :members
 ("package"
  "basics"
  "lispworks"						; implementation-specific
  "shared"
  #+win32 "win32-ffi"
  #+win32 "win32-shared")
 :rules
 ((:in-order-to :compile :all (:caused-by (:compile :previous))
   (:requires (:load :previous)))
  (:in-order-to :load :all
   (:requires (:load :previous))) ))

;(lw:compile-system 'YSTOK-FFC :load t :force nil)

