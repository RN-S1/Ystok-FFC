;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - LispWorks specific code
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conventions and notes
;;; - We follow LispWorks general design priciples.
;;; - We prefer Allegro's :null-terminate key parameter name to LW's :null-terminated-p.

(in-package :ystok.ffc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  EXTERNAL FORMATS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Underlying foreign type for the string pseudo-type in with-foreign
;; For :wchar-t, we always assume UCS-2 to be the corresponding external format.
(defparameter *foreign-char-type* :wchar-t)

;; Default converting arguments and/or values for OS APIs
(defvar *foreign-external-format*)

;; External format and code page assumed for the :char type and :c-string pseudo-type.
;; Used for encoding foreign C strings (within with-foreign).
(defparameter *char-external-format* :latin-1)
(defparameter *char-code-page* :latin-1)			 	; Windows only?

(defun %initialize-ffc (&key (default #+win32 (if (string= (software-type) "Windows NT")
                                                  :unicode		; = UCS-2 (UTF-16)
                                                  win32:*multibyte-code-page-ef*)
                                             #-win32 :latin-1)		; ascii?
                             (char #+win32 win32:*multibyte-code-page-ef*
                                   #-win32 :latin-1))
  ;; Set C locale, e.g. => "Russian_Russia.1251". 
  ;; Q: As we always convert foreign memory objects passed to foreign routines manually,
  ;;    shall we call fli:set-locale at all? Maybe it used for functions named "...A"?
  (fli:set-locale)
  (setq *foreign-external-format* default
        *char-external-format* char
        *char-code-page* #+win32 (if (consp char)
                                     (getf (rest win32:*multibyte-code-page-ef*) :id)
                                     char)
                         #-win32 char))

(%initialize-ffc)
(lw:define-action "When starting image" "Initialize FFC locale" '%initialize-ffc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LIBRARIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; On LispWorks, modules are loaded automatically when the :module argument
;;; is given to fli:define-foreign-variable/function

(defun load-foreign-module (name &key (real-name (module-real-name name))
                                      (lifetime :indefinite))
                                      ;(connection-style :automatic)
  (fli:register-module name
                       :real-name (if (pathnamep real-name)
                                      (namestring real-name)
                                      real-name)
                       :lifetime lifetime))

(defun module-loaded-pathname (module &key real-name)
 ;;; Value: Pathname (truename on LWW);
  ;;        NIL if not loaded or loaded but not connected.
  (declare (ignore real-name))
  (fli:connected-module-pathname module))

(defun unload-foreign-module (name &key real-name (remove t))
 ;;; Value: True on success (or only if has been connected?)
  (declare (ignore real-name))
  (fli:disconnect-module name :remove remove)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TYPES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-type (name type)
  `(fli:define-c-typedef ,name ,type))

#-win32 (fli:define-c-typedef WORD  :unsigned-short)
#-win32 (fli:define-c-typedef DWORD :unsigned-long)

;; BOOL   -> :boolean
;; BYTE   -> :unsigned-byte
;; USHORT -> :unsigned-short
;; VOID   -> :void

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  POINTERS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OT1H, %null-pointer-p: nil => false, i.e. should not signal on null argument.
;;; OTOH, %pointer-eql accepts only pointer arguments.

#+lispworks4.3
(defconstant *null-pointer* (fli:make-pointer :address 0 :type :void))

(setf (fdefinition '%pointerp)       (fdefinition 'fli:pointerp)
      (fdefinition '%null-pointer-p) (fdefinition 'fli:null-pointer-p)
      (fdefinition '%pointer-eql)    (fdefinition 'fli:pointer-eq)

      (fdefinition '%pointer-address)      (fdefinition 'fli:pointer-address)
      (fdefinition '%pointer-element-size) (fdefinition 'fli:pointer-element-size)
      (fdefinition '%size-of)              (fdefinition 'fli:size-of))

#+unused
(defmacro %address-pointer (address &optional (type :void))
 ;;; Args: type Not evaluated (if NIL is passed, LW assumes the :function type).
 `(fli:make-pointer :address ,address :type ',type))

(defmacro %copy-pointer (pointer &optional offset)
 ;;; In-heap copy of the pinter given
  (if offset
      `(fli:incf-pointer (fli:copy-pointer ,pointer) ,offset)
      `(fli:copy-pointer ,pointer)))

(defmacro %incf-pointer (pointer &optional offset)
  `(fli:incf-pointer ,pointer ,@(when offset `(,offset))))

(defmacro %decf-pointer (pointer &optional offset)
  `(fli:decf-pointer ,pointer ,@(when offset `(,offset))))

(defmacro with-pointer ((ptr &key index type) pointer &body body)
 ;;; Bind the ptr variable locally to in-stack copy of the pointer given,
  ;; increase by index, and evaluate the body.
  ;; Args: index  Index within a coerced pointer, evaluated.
  ;;       type   Not evaluated (is never passed from YstokSQL).
  ;; NB: Should not cons.
  `(fli:with-coerced-pointer (,ptr ,@(when type `(:type ',type))) ,pointer
     ,@(when index `((fli:incf-pointer ,ptr ,index)))
     ,@body))

(defmacro %alloc (type &optional (nelems 1) initial-element dynamic)
 ;;; Generaly used to allocate a static FLI object
  ;; Args: type            Immediate (scalar) or structure type, not evaluated.
  ;;       nelems          The number of elements to allocate, evaluated.
  ;;       initial-element Only allowed if type is scalar, evaluated.
  ;;       dynamic         True is only allowed
  ;;                       - when called in the scope of with-foreign,
  ;;			   - when passed from within ystok-ffc itself.
  (if dynamic
      `(fli:allocate-dynamic-foreign-object :type ',type :nelems ,nelems
        ,@(when initial-element `(,:initial-element ,initial-element)))
      `(fli:allocate-foreign-object :type ',type :nelems ,nelems
        ,@(when initial-element `(,:initial-element ,initial-element)))))

(declaim (inline %free))
(defun %free (pointer)
 ;;; Args: pointer Points to an object allocated in heap.
  ;;               When *null-pointer* is passed, the function must do nothing.
  (fli:free-foreign-object pointer))

(defmacro %deref (pointer &optional index)
  `(fli:dereference ,pointer ,@(when index `(,:index ,index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  VECTOR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Args: type  Element type, evaluated

(defmacro %alloc-vector (type nelems)
 ;;; Args: type Element type, not array type, not evaluated
  `(fli:allocate-foreign-object :type (list :c-array ',type ,nelems)))

(declaim (inline %length))
(defun %length (pointer)
  (first (fli:foreign-array-dimensions pointer)))

(defmacro %svref (array index &key type)
 ;;; Args: type  Element-type, not evaluated and must produce (translated by LW to) one of
  ;;		  (unsigned-byte 32), (signed-byte 32),
  ;;		  (unsigned-byte 16), (signed-byte 16),
  ;;		  (unsigned-byte 8), (signed-byte 8),
  ;;		  double-float, or single-float.
  ;;		 If supplied, the array pointer is casted to this type first.
  ;;       index Subscript within the given or casted array.
  ;; LW4.4 Documentation BAD (amended for LW6):
  ;;	fli:foreign-typed-aref requires byte-offset as index argument.
  #-lispworks4.3
  (if type
      (let ((n (fli:size-of type)))
        `(fli:foreign-typed-aref ',type ,array ,(if (eql n 1) index `(* ,index ,n))))
      `(fli:foreign-aref ,array ,index))
  #+lispworks4.3
  `(fli:foreign-aref ,array ,index))			; type and bounds checking

(defmacro %svref-pointer (array index)
 ;;; Pointer to the specified element
  `(fli:foreign-array-pointer ,array ,index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-struct (name &rest slots)
 `(dspec:def (define-foreign-struct ,name)
    (fli:define-c-struct ,name ,@slots)))

;(dspec:define-dspec-alias define-foreign-struct (name) `(fli:define-foreign-type ,name))

(defmacro %slot (pointer structure-type slot &key type copy-foreign-object)
 ;;; Args: pointer        Evaluated.
  ;;       structure-type Type of structure.
  ;;       slot           Symbol (a path of symbols for LW6.1 is not supported).
  ;;       type           Type of the resulting value.
  ;;       structure-type, slot, type are not evaluated.
  ;; NB: Specifying structure-type results in compiled code that (for both read and write)
  ;;	 1.9 times larger
  ;;	 5.0 times faster
 `(fli:foreign-slot-value ,pointer ',slot
                          ,@(when structure-type `(,:object-type ',structure-type))
                          ,@(when type `(,:type ',type))	; value type
                          ,@(when copy-foreign-object `(,:copy-foreign-object
                                                        ,copy-foreign-object)) ))

(defmacro %slot-pointer (pointer structure-type slot &key type)
 ;;; Args: structure-type, slot, type - are not evaluated.
 `(fli:foreign-slot-pointer ,pointer ',slot
                            ,@(when structure-type `(,:object-type ',structure-type))
                            ,@(when type `(,:type ',type))))	; value type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ELEMENTARY TYPES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing memory no matter what the type of the pointer is.

(defmacro %get-octet (pointer index)
  `(fli:dereference ,pointer :type :unsigned-byte :index ,index))

(defmacro %get-word (pointer &optional index)
  `(fli:dereference ,pointer :type :unsigned-short ,@(when index `(:index ,index))))

(defmacro %get-dword (pointer &optional index)
  `(fli:dereference ,pointer :type :unsigned-long ,@(when index `(:index ,index))))

(declaim (ftype (function (t symbol fixnum &optional fixnum) t)
                %get-binary))
(defun %get-binary (pointer result-type octet-length &optional (index 0))
 ;;; Copy from foreign memory to a newly created lisp object.
  ;; Args: pointer      Preallocated of arbitrary type.
  ;;       result-type  Is one of:
  ;;                    bit-vector,
  ;;                    string - hex string,
  ;;                    integer - unsigned, stored in little endian
  ;;                    (e.g. YstokSQL :bookmark).
  ;;                    otherwise - means simple-octet-vector,
  ;;       octet-length Number of bytes to read.
  ;;       index        Subscript in pointer.
  ;; Value: A vector (a simple array) in accordance with the result-type.
  (declare (optimize (speed 3) (space 0) #-debug (safety 0) #-debug (debug 0)
                     #-debug (hcl:fixnum-safety 0))
           (fixnum octet-length index))
  (fli:with-coerced-pointer (ptr :type :unsigned-byte) pointer
    (case result-type
      ((bit-vector simple-bit-vector)
       ;; For readability, MS bit of every octet is put on the left,
       ;; i.e. with a smaller subscript.
       ;; Старший бит из каждого байта помещается левее, т.е. с меньшим индексом
       ;; в битовом векторе. Как следствие, печатное представление вектора нагляднее.
       (fli:incf-pointer ptr index)
       (loop with vector = (make-array (ash octet-length 3) :element-type 'bit)
             for i of-type fixnum upfrom 0 below octet-length
             for byte of-type fixnum = (fli:dereference ptr)
             do (dotimes (j 8)
                  (declare (fixnum j))
                  (setf (sbit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))
                (fli:incf-pointer ptr)
             finally (return vector)))
      (string 							; a la object-hex-string
       (fli:incf-pointer ptr index)
       (loop with string = (make-string (* octet-length 2) :element-type 'base-char)
             repeat octet-length
             for j of-type fixnum upfrom 0 by 2
             for byte of-type fixnum = (fli:dereference ptr)
             do (setf (schar string j) (digit-char (ash byte -4) 16)
                      (schar string (1+ j)) (digit-char (logand byte #x0F) 16))
                (fli:incf-pointer ptr)
             finally (return string)))
      ((integer :bookmark)					; used by YstokSQL
       ;; Assuming little-endian, i.e. when the less significant byte goes first;
       ;; we can get either a fixnum or bignum as a result.
       (fli:incf-pointer ptr index)
       (loop with n of-type integer = 0				; unsigned
             for byte-position of-type fixnum upfrom 0 by 8
             repeat octet-length
             ;for bignum of-type integer = (fli:dereference ptr :type :signed-byte)
             ;           then (logior (ash bignum 8) (fli:dereference ptr))
             do ;(setf bignum (+ (ash bignum 8) (fli:dereference ptr)))
                (setq n (dpb (fli:dereference ptr) (byte 8 byte-position) n))
                (fli:incf-pointer ptr)
             finally (return n)))
      (otherwise						; vector = octet-vector
       (fli:replace-foreign-array (make-array octet-length :element-type'(unsigned-byte 8))
                                  ptr
                                  :start2 index :end2 (+ index octet-length)) ))))

(declaim (ftype (function (t t fixnum &key (:start fixnum) (:index fixnum)) fixnum)
                %get-binary))
(defun %put-binary (pointer data max-octet-length &key (start 0) (index 0))
 ;;; Copy from the lisp object DATA to foreign memory.
  ;; Arg: pointer Preallocated of arbitrary type.
  ;;      data    One of:
  ;;              - secialized octet vector (simple or not),
  ;;              - bit vector (may be displaced or with a fill pointer),
  ;;              - integer.
  ;;      start   Subscript in data (only used when data is an octet-vector).
  ;;      index   Subscript in pointer.
  ;; Value: The number of octets that has been actually put.
  (declare (optimize (speed 3) (space 0) #-debug (safety 0)
                     #+(and lispworks (not debug)) (hcl:fixnum-safety 0))
           (fixnum max-octet-length start index))
  (fli:with-coerced-pointer (ptr :type :unsigned-byte) pointer
    (fli:incf-pointer ptr index)
    (cond ((bit-vector-p data)		
           #+debug (assert (zerop start) (start)
                     "Bit-vector can only be put into BINARY buffer without chunking.")
           (let* ((bit-count (length data))
                  (octet-length (ceiling bit-count 8)))
             (declare (fixnum bit-count octet-length))
             ;(when (and max-octet-length (> octet-length max-octet-length))
             ;  (error "bit vector of length ~d is longer than max-octet-length: ~d"
             ;         bit-count (* max-octet-length 8)))
             (loop for i of-type fixnum upfrom 0 below octet-length
                   and byte of-type (unsigned-byte 8) = 0 
                   do
                   (loop for j of-type fixnum upfrom 0 below 8
                         for index of-type fixnum = (+ (ash i 3) j)
                         while (< index bit-count)
                         do (setf byte (logior byte (ash (sbit data index) (- 7 j)))))
                   (setf (fli:dereference ptr) byte)
                   (fli:incf-pointer ptr))
             octet-length))
          ((and (vectorp data)						; octet-vector-p
                (equal (array-element-type data) '(unsigned-byte 8)))
           (let ((octet-length (the fixnum (min (the fixnum (- (length data) start))
                                                max-octet-length))))
             (declare (fixnum octet-length))
             (if (< 0 octet-length)
                 (progn (fli:replace-foreign-array ptr data		;:start1 index
                                                   :end1 octet-length	;(+ index)
                                                   :start2 start)
                   octet-length)
                 0)))
          ((integerp data)					; bookmark in YstokSQL
           ;; We fill all the reserved space with trailing zeros because its length
           ;; cannot be addapted to the integer length.
           ;(assert (zerop start) (start)
           ;  "Bookmarks can only be put into BINARY buffer without chunking.")
           (loop repeat max-octet-length
                 for byte-position of-type fixnum upfrom 0 by 8
                 do (setf (fli:dereference ptr) (ldb (byte 8 byte-position) data))
                    (fli:incf-pointer ptr))
                 ; sum 1))
           max-octet-length)
          (t (error "%PUT-BINARY: The type of DATA is not supported.")))))

(defmacro %fill-binary (pointer octet-length &optional byte)
 ;;; Fill OCTET-LENGTH octets in foreign memory area pointed by POINTER with BYTE.
  ;; Args: pointer  Pointer to an arbitrary (or integral?) type.
  ;;	   byte     Defaults to 0.
  ;; NB: fill-foreign-object neither seems to respect actual array pointer size nor nelems
  `(fli:fill-foreign-object ,pointer :nelems ,octet-length ,@(when byte `(:byte ,byte))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Args: pointer Of type :char or :wchar-t
;;;       type ::= :char | :wchar-t - not evaluated.
;;; Value/char: Lisp character object.

(defmacro %get-char (pointer &optional index type)	;*foreign-char-type*
  `(fli:dereference ,pointer ,@(when index `(,:index ,index))
                    ,@(when type `(,:type ',type))))
(defmacro %put-char (pointer char &optional index type)
  `(setf (%get-char ,pointer ,index ,type) ,char))

(declaim (ftype (function (t &optional t) fixnum) %cstring-length)
         (inline %cstring-length))
(defun %cstring-length (pointer &optional (external-format *foreign-external-format*))
                                          ;(type *foreign-char-type*)
 ;;; Args: pointer         Null-terminated C string.
  ;;       external-format MUST NOT be :UTF-8.
  ;; Value: Number of characters excluding the #\Null terminator.
  (declare #-debug (optimize (speed 3) (safety 0) (debug 0) (hcl:fixnum-safety 0))
           #+debug (optimize (debug 3) (safety 3)))
  #+debug (assert (not (eq external-format :utf-8)))
  (fli:with-coerced-pointer (ptr :type (if (eq external-format :unicode) :wchar-t :char))
                                           ;(or (memq external-format '(:ascii :latin-1))
                                           ;    #+win32 (consp external-format))
                                           
      pointer
    (the fixnum (loop until (char= (fli:dereference ptr) #\Null)
                      count (%incf-pointer ptr)))))

(declaim (inline %lisp-cstring))
(defun %lisp-cstring (string &key (external-format *foreign-external-format*)
                                  (null-terminate t)
                                  dynamic)
 ;;; Copy string into freshly allocated null-terminated C-string 
  ;; Args: string          Input Lisp string or NIL.
  ;;       external-format :unicode or whatever.
  ;;       null-terminate  True means add trailing 0 to the foreigh string.
  ;;       dynamic         Detemines what memory: static or dynamic
  ;;                       True is only allowed
  ;;                       - when called in the scope of with-foreign,
  ;;			   - when passed from within ystok-ffc itself.
  ;; Value: 1) pointer of the foriegn type corresponding to the external-format
  ;;        2) length (including trailing #\Null)
  ;;        3) octet-length
  ;; CAUTION: Using :dynamic t is hardly compatible among CL implementaions.
  ;; CAUTION: SBCL returns only octet-length as the second value!
  ;; LW BAD: Inefficient on LW prior to 5.0.
  (fli:convert-to-foreign-string string
                                 :external-format external-format
                                 :allocation (if dynamic :dynamic nil)
                                 :null-terminated-p null-terminate
                                 :allow-null t))

(defun %cstring-lisp (pointer &key length (external-format *foreign-external-format*))
 ;;; Create new Lisp string with the element-type according to external format
  ;; Args: pointer         Pointer to C string (of an arbitrary type)
  ;;       length          Fixnum or FLI pointer to an integer value
  ;;                       - in characters for single-byte or two-byte ef like UCS-2,
  ;;                       - in octets for UTF-8 (where is it passed from for UTF-8?)
  ;;                       or NIL if the poitner refers to a null-terminated buffer.
  ;;       external-format :Unicode or whatever.
  ;; Value: New Lisp string or NIL.
  ;; NB1: The actual type (:char or :wchar-t) is determined from the external-format.
  (declare #-debug (optimize (speed 3) (safety 0) (debug 0) (hcl:fixnum-safety 0))
           #+debug (optimize (debug 3) (safety 3)))
  (if (%null-pointer-p pointer)
      nil
    #+lispworks4
    (case external-format
      (:unicode
       (let ((length #1=(cond ((numberp length) length)
                              ((%pointerp length) (%deref length))
                              (t (%cstring-length pointer external-format)))))
         (declare (fixnum length))
         (with-pointer (ptr :type :wchar-t :index length) pointer
           (do ((str (make-string length :element-type 'lw:simple-char))) ; (ef-type ...)
               ((zerop length)
                str)
             (declare (type simple-string str))
             (fli:decf-pointer ptr)
             (setf (lw:stchar str (decf length)) (fli:dereference ptr))))))
      ((:ascii :latin-1)
       (let ((length #1#))
         (declare (fixnum length))
         (with-pointer (ptr :type :char :index length) pointer
           (do ((str (make-string length :element-type 'base-char)))
               ((zerop length)
                str)
             (declare (type simple-base-string str))
             (fli:decf-pointer ptr)
             (setf (schar str (decf length)) (fli:dereference ptr))))))
      (:utf-8							; a la octets-to-string
       (with-output-to-string (stream nil :element-type 'lw:simple-char)
         (do ((chars-remaining 0)
              (accumulator 0)
              (end (cond ((numberp length) length)		; octet-length actually
                         ((%pointerp length) (%deref length))
                         (t most-positive-fixnum)))
              (from-index 0 (1+ from-index)))
             ((>= from-index end)
              (unless (= chars-remaining 0)
                (error "Unexpected end of UTF-8 sequence.")))
           (declare (fixnum chars-remaining accumulator end from-index))
           (let ((octet (%get-octet pointer from-index)))
              (declare (type (unsigned-byte 8) octet))
              (cond ((= octet 0)
                     (return))
                    ((= chars-remaining 0)
                     (cond ((= (logand octet #b10000000) 0)
                            ;(if (and (= octet 13) crlf
                            ;         (< (1+ from-index) end)
                            ;         (= (%get-octet ptr (1+ from-index)) 10))
                            ;    (progn (write-char #\Newline stream)
                            ;      (incf from-index))
                            (write-char (code-char octet) stream))
                           ((= (logand octet #b11100000) #b11000000)
                            (setq accumulator (logand octet #b00011111)
                                  chars-remaining 1))
                           ((= (logand octet #b11110000) #b11100000)
                            (setq accumulator (logand octet #b00001111)
                                  chars-remaining 2))
                           ((= (logand octet #b11111000) #b11110000)
                            (setq accumulator (logand octet #b00000111)
                                  chars-remaining 3))
                           ((= (logand octet #b11111100) #b11111000)
                            (setq accumulator (logand octet #b00000011)
                                  chars-remaining 4))
                           ((= (logand octet #b11100000) #b11111110)
                            (setq accumulator (logand octet #b00000001)
                                  chars-remaining 5))
                           (t
                   #2=(error "Invalid octet #x~2,'0x at position ~d in UTF-8 sequence."
                             octet from-index))))
                    ((= (logand octet #b11000000) #b10000000)
                     (setq accumulator (logior (ash accumulator 6)
                                               (logand octet #b00111111)))
                     (when (= (decf chars-remaining) 0)
                       (write-char (code-char accumulator) stream)))
                    (t #2#))))))
      (otherwise
       (if (consp external-format)
           (with-pointer (ptr :type :unsigned-byte :index length) pointer
             (do* ((length #1#)
                   (code-page (getf (rest external-format) :id))
                   (str (make-string length :element-type (ef:external-format-type
                                                           external-format))))
                  ((zerop length)
                   str)
               (declare (fixnum length)
                        (type simple-string str))
               (fli:decf-pointer ptr)
               (setf (schar str (decf length))
                     (ef:find-external-char (fli:dereference ptr) code-page))))
           ;; For other formats, use the built-in (rather inefficient in LW prior 5.0)
           ;; BAD: The doc is vague whether the length is in characters or in octets.
           (fli:convert-from-foreign-string pointer
               :length (cond ((numberp length) length)
                             ((%pointerp length) (%deref length)))
               :null-terminated-p (not length)
               :external-format external-format))) )
    #-lispworks4
    (fli:convert-from-foreign-string pointer
        :length (cond ((numberp length) length)
                      ((%pointerp length) (%deref length)))
        :null-terminated-p (not length)
        :external-format external-format)))

(defmacro with-cstring ((pointer string &key length octet-length
                                             (external-format '*foreign-external-format*)
                                             (null-terminate (null length)))
                        &body body)
 ;;; Allocate memory for a C-string, execute the body, then deallocate the C-string
  ;; Args: pointer Symbol bound to a pointer to the foreign string.
  ;;       string  Input Lisp string or NIL.
  ;;       length  Variable bound to resulting string length in characters.
  ;;       octet-length
  ;;               Variable bound to number of bytes occupied by the C-string.
  ;;       external-format
  ;;               Imlementation specific e.g. :unicode or :latin-1
  ;;               defaults to *foreign-external-format*.
  ;;       null-terminate
  ;;               If false, C-string will not be terminated by the #\Null char.
  ;;               Terminating may be not needed if an explicit length is also
  ;;               passed to the foreign routine.
  ;; NB:    Outer (with-foreign () ...) is not needed.
  ;; LW NB: The proper external-format guarantees that pointer will be of proper char type.
  `(fli:with-foreign-string (,pointer
                             ,(or length 'length)
                             ,(or octet-length 'octet-length)
                             ,:external-format ,external-format
                             ,:null-terminated-p ,null-terminate
                             ,:allow-null t)
       ,string
     (declare (ignorable ,@(unless length '(length))
                         ,@(unless octet-length '(octet-length))))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CONVENIENCE MACROS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-function (name params result-type &rest args 
                                   &key (module '*default-foreign-module*)
                                        (encoding :source)
                                        (calling-convention :stdcall)
                                        ;(no-check nil nc-supplied-p)
                                   &allow-other-keys)
 ;;; Args: args     Additionally supported :documentaion, :no-check.
  ;;       encoding :dbcs means ...W entry points of Win32 API code.
  ;;       module   Though accepted for Linux, FreeBSD, or x86/x64 Solar, but not for UNIX.
  (let ((lisp-name (cond ((consp name) (first name))
                         ((stringp name) (intern (string-upcase name))) ; YSQL -> *package*
                         (t name)))
        (c-name (cond ((consp name) (second name))
                      ((stringp name) name)				; YSQL
                      (t (symbol-name name)))))
    `(dspec:def (define-foreign-function ,lisp-name)
       (fli:define-foreign-function ,(list lisp-name c-name encoding)   ; :dbcs
         ,params
         :result-type ,result-type
         :language :ansi-c
         :module ,module
         :calling-convention ,calling-convention
         ,@(system::remove-properties args '(:module :encoding :calling-convention))))))
         ;,@(when nc-supplied-p `(:no-check ,no-check))

(dspec:define-form-parser define-foreign-function (name)
 `(,define-foreign-function ,(cond ((consp name) (first name))
                                   ((stringp name) (intern (string-upcase name)))
                                   (t name))))

(defmacro define-foreign-variable (name type &rest args
                                   &key (module '*default-foreign-module*)
                                   &allow-other-keys)
 ;;; Args: name ::= lisp-name | (lisp-name c-name-string)
  ;;       accessor ::= :value       Default for nog-aggregate type
  ;;                  | :address-of  If the variable should be of type (:pointer type)
  ;; NB: We define both an accessor function and symbol macro that expands to (accessor).
  (let ((lisp-name (cond ((consp name) (first name))
                         ((stringp name) (intern (string-upcase name))) ; YSQL -> *package*
                         (t name)))
        (c-name (cond ((consp name) (second name))
                      ((stringp name) name)				; YSQL
                      (t (symbol-name name)))))
    `(dspec:def (define-foreign-variable ,name)
       (fli:define-foreign-variable (,lisp-name ,c-name)
           :type ,type
           :module ,module
           ,@(system::remove-properties args '(:module)))
       (define-symbol-macro ,lisp-name (,lisp-name)))))
       ;(fli:dereference (,(first-or-self name)) :copy-foreign-object nil))))

(defmacro with-foreign (bindings &body body)
 ;;; Allocate memory for foreign objects, execute the body, then deallocate the objects
  ;; Args: binding ::= (var foreign-type &key nelems initial-element initial-contents)
  ;;       foreign-type
  ;;         Not evaluated and is
  ;;         - either a designator of a defined foreign type, built-in or composite,
  ;;         - or one of the symbols for converting by %lisp-cstring or allocate-dynamic...
  ;;           :c-string     - means base string (:char)
  ;;           :ef-wc-string - means UCS-2 string (:wchar-t)
  ;;           string        - according to (run-time) value of *foreign-char-type* and
  ;;                           *foreign-external-format*.
  ;;       nelems
  ;;         Evaluated, means allocating an area (not foreing vector!) of this length.
  ;;       initial-element
  ;;         Evaluated, as a rule used when nelems is not null and not for string types.
  ;;       initial-contents
  ;;         Evaluated, can only be specified when nelems is null.
  ;;         For string pseudo-types, must be a lisp string, not a character!
  ;; NB: In many cases, only one of :nelems or :initial-contents is enough.
  ;;     For string types, exactly one of :initial-contents on :nelems is allowed.
  ;; CAUTION:
  ;;   All the variables are considered to be bound sequentially (as with let*),
  ;;   immediate (simple) types are bound before composite as a rule.
  ;; LW NB:
  ;;   The fli:with-dynamic-foreign-objects macro supports
  ;;    LW prior 5.1
  ;;      Only scalar types and &optional initial-element
  ;;    LW 5.1 and later
  ;;      Any types and &key :nelems :initial-element :initial-contents :fill :size-slot
  ;;      Only :nelems and :initial-element must be used with YstokFFC!
  ;; Compatibility NB:
  ;;   YSQL/FFC only supported string allocation but not initialization.
  (let ((simple ())
        (complex ())) 
    (dolist (binding bindings)
      (destructuring-bind (var type &key nelems initial-element
                                         initial-contents (null-terminate t supplied-p))
          binding
       (flet ((%string (type &optional external-format)
                ;; Args: type Foreign type
                (cond (initial-contents
                       (if nelems
                           (error "Both NELEMS and INITIAL-CONTENTS specified for type ~s."
                                  type)
                           (push `(,var (%lisp-cstring ,initial-contents
                                           ,@(when external-format
                                               `(,:external-format ,external-format))
                                           ,@(when supplied-p
                                               `(,:null-terminate ,null-terminate))
                                           :dynamic t))
                                 complex)))
                      (nelems
                       (push `(,var (fli:allocate-dynamic-foreign-object
                                        :type ,type
                                        :nelems ,nelems
                                        #1=,@(when initial-element
                                               `(,:initial-element ,initial-element))))
                             complex))
                      (t
                       (error "Neither NELEMS nor INITIAL-CONTENTS specified for type ~s."
                              type)))))
        (case type
          (string						; default external format
           (%string '*foreign-char-type*))			; both args are run-time 
          (:c-string
           (%string :char '*char-external-format*))
          (:ef-wc-string
           (%string :wchar-t :unicode))
          (otherwise
	   ;; Non-character type - no external-format conversion
           #-(or lispworks4 lispworks5.0)
           (push binding simple)				; let LW5.1 parse itself
           #+(or lispworks4 lispworks5.0)
           (if nelems
               (push `(,var (fli:allocate-dynamic-foreign-object :type ',type
                                                                 :nelems ,nelems
                                                                 #1#))
                     complex)
               (push `(,var ,type ,@(cond (initial-contents `(,initial-contents))
                                          (initial-element `(,initial-element))))
                                          
                     simple))) ))))
    `(fli:with-dynamic-foreign-objects ,simple
       ,@(if complex
             `((let ,(nreverse complex)
               ,@body))
             body))))
