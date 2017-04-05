;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Implementation-independent stuff
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :ystok.ffc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ELEMENTARY TYPES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %put-octet (pointer octet index)
  `(setf (%get-octet ,pointer ,index) ,octet))

(defmacro %put-word (pointer word &optional index)
  `(setf (%get-word ,pointer ,index) ,word))

(defmacro %put-dword (pointer dword &optional index)
  `(setf (%get-dword ,pointer ,index) ,dword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %null-terminate (arg)
 ;;; Append #\Null char to the string if it is not null-terminated yet.
  ;; Args: arg  Lisp string or list of strings.
  ;; Value: NIL if arg is not cons or string.
  ;; NB: When accompanied by with-cstring, produces a C string array (REG_MULTI_SZ),
  ;;     i.e. the resulting buffer will contain two #\Null character at the end.
  (let* ((string (cond ((consp arg)				; multi-string
                        (apply #'concatenate 'string ;(interleave-seq 'string #\Null arg)
                               (loop with prev = nil
                                     for elt in arg
                                     when elt
                                       when prev collect #1=#.(string #\Null) end
                                       and collect elt
                                       and do (setq prev t)
                                     end)))
                       ((stringp arg) arg)
                       (t (return-from %null-terminate nil))))
         (length (length string)))
    (if (or (zerop length) (char/= (char string (1- length)) #\Null))
        (concatenate 'string string #1#)
        string)))

(defun %multi-sz-list (pointer &key length (external-format *foreign-external-format*))
 ;;; Convert multi-string (REG_MULTI_SZ) to a list of strings.
  ;; Args: pointer Pointer of :char or :wchar-t pointing to a buffer with an array
  ;;               of strings interleaved with #\Null and terminated with double #\Null.
  ;;       length  Integer or pointer to DWORD that contain the total number
  ;;               of characters placed into the buffer, including terminator.
  ;; Value: List of Lisp strings, empty strings "" are exclued.
  ;; CAUTION: When length <=2, buggy WinAPI functions can place garbage into the buffer!
  (declare #-debug (optimize (speed 3) (safety 0) (debug 0)
                             #+lispworks (hcl:fixnum-safety 0))
           #+debug (optimize (debug 3) (safety 3)))
  (if length
      (if (<= (if (%pointerp length) (setq length (%deref length)) length) 2)
          ()
          #+lispworks
          (lw:split-sequence #.(string #\Null) (%cstring-lisp pointer :length length)
                             :coalesce-separators t)
          #-lispworks			; a la (yl:split-seq #\Null (%cstring-lisp ...))
          (loop with seq = (%cstring-lisp pointer :length length)
                with length of-type fixnum = (length seq)
                for left of-type fixnum = 0 then (+ right 1)
                for right of-type fixnum = (or (position #\Null seq :start left) length)
                when (< left right)
                collect (subseq seq left right)
                until (<= length right)))
      (with-pointer (ptr) pointer			;:type *foreign-char-type*)
        (loop until (= (%deref ptr) 0)			;(or (and length (<= length count))
              for string = (%cstring-lisp ptr :external-format external-format)
              for offset = (1+ (length string))
              collect string
              do (%incf-pointer ptr offset)))))
