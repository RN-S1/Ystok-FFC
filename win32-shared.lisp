;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Windows API helpers
;;; Copyright (c) 2003-2014 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal and top-level function calling Win32 API routines.

(in-package :ystok.ffc)

(defun %wstring-upper-down-case (string start end upper-case-p)
 ;;; Args: string     Unicode USC-2 string, simple or not.
  ;;       start, end Indicate the substring that needs to be converted
  ;; Value: Always a copy of the string.
  ;; CAUTION: Does not work on Windows 98.
  ;; LW NB:
  ;;  1) Using fli:foreign-typed-aref seems to give no benefit in speed.
  ;;  2) Use win32::char-lower$string instead?
  (declare (type string string)
           (fixnum start)
           (optimize (speed 3) (debug 0) (safety 0) #+lispworks (hcl:fixnum-safety 0)))
  (let* ((length (length string))
         (end (or end length))
         (size (- end start)))
    (declare (fixnum length end size))
    (with-foreign ((ptr :wchar-t :nelems size))
      (do ((i start (1+ i))
           (j 0 (1+ j)))	; (+ j 2) if j is an offset in octets
          ((>= i end))
        (declare (fixnum i j))
        (%put-char ptr (char string i) j :wchar-t))
				; (setf (fli:foreign-typed-aref '(unsigned-byte 16) ptr j)
				;       (char-code (char string i)))
      (if upper-case-p
          (|CharUpperBuff| ptr size)
          (|CharLowerBuff| ptr size))
      (let ((value (make-string length #+lispworks #+lispworks
                                       :element-type 'lw:simple-char)))
        (declare (type #+lispworks lw:simple-text-string
                       #-lispworks simple-string
                       value))
        (macrolet ((%setchar (i char) 
                     `(setf (#+lispworks lw:stchar #-lispworks schar value ,i) ,char)))
          (dotimes (i start)					; copy prefix as is
            (declare (fixnum i))
            (%setchar i (char string i)))
          (do ((i start (1+ i))					; copy converted middle
               (j 0 (1+ j))) ;(+ j 2)))
              ((>= i end))
            (declare (fixnum i j))
            (%setchar i (%get-char ptr j :wchar-t)))
				; (code-char (fli:foreign-typed-aref
				;            '(unsigned-byte 16) ptr j)))
          (do ((i end (1+ i)))					; copy suffix as is
              ((>= i length))
            (declare (fixnum i))
            (%setchar i (char string i))))
        value))))

(defun get-locale-info (info-type &optional (locale :user))
  (with-foreign ((buffer :ef-wc-string :nelems #1=64))
    (let ((length (|GetLocaleInfo| (case locale
                                     (:user LOCALE_USER_DEFAULT)
                                     (:system LOCALE_SYSTEM_DEFAULT)
                                     (otherwise locale))
                                   info-type buffer #1#)))
      (unless (zerop length)
        ;; Length usually counts the #\Null terminator
        (%cstring-lisp buffer
                       :length (if (char= (%get-char buffer (1- length) :wchar-t) #\Null)
                                   (1- length)
                                   length)
                       :external-format #+lispworks :unicode
                                        #+(and sbcl little-endian) :ucs-2le
                                        #+(and sbcl big-endian) :ucs-2be)))))

