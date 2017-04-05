;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - SBCL specific code
;;; Copyright (c) 2002-2014 Dr. Dmitry Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;; - In contrast to CFFI, we rely on SBCL typed pointers (of type alien-value)
;;;   rather than untyped SAPs.
;;; - Foreign types passed to macros are assumed to be as for LispWorks.
;;; - External format specifiers are assumed to be as for SBCL.
;;; - Void pointers are prefered in FFI declaration where foreign string are passed.
;;; - Temporal allocation of foreign strings is not optimal: in heap rather than in stack.
;;; - The following declarations are avoided as they leads to the compiler note:
;;;      (dynamic-extent sap) -> Cannot allocate in stack.
;;; CAUTION
;;;   Not tested on SBCL built without Unicode support.

(in-package :ystok.ffc)

;; Muffle compiler-notes globally
;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;(declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  EXTERNAL FORMATS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Underlying foreign type for the string pseudo-type in with-foreign (and with-cstring?)
;; For :wchar-t, we always assume UCS-2 to be the corresponding external format
;; CAUTION:
;;  In macroexpansion of with-cstring and with-foreign we rely on compile-time
;;  value of *foreign-char-type* rather than run-time (as for LispWorks).
(defparameter *foreign-char-type* #+sb-unicode :wchar-t #-sb-unicode :char)

;; Default converting arguments and/or values for OS APIs
(defvar *foreign-external-format*)

;; External format assumed for the :char type and :c-string pseudo-type.
;; Used for encoding foreign C strings (within with-foreign).

(defparameter *char-external-format* :latin-1)
;(defparameter *char-code-page* 28591)				 	; Windows only?

(defun %initialize-ffc (&key (default #+(and win32 sb-unicode little-endian)
                                      :ucs-2le
                                      #+(and win32 sb-unicode big-endian)
                                      :ucs-2be
                                      #-(and win32 sb-unicode)
                                      sb-impl::*default-external-format*)
                             (char sb-alien::*default-c-string-external-format*))
 ;;; Windows NB:
  ;;   We rely on 16-bit WCHAR_T by default, i.e. link to ..W not ..A DLL entry points.
  ;;   Without sb-unicode, both the arguments default to the code-page, e.g. :CP1251.
  (setq *foreign-external-format* default
        *char-external-format* char))
        #|*char-code-page* (loop for key being each hash-key		; retrieve key
                                 in SB-WIN32::*CODEPAGE-TO-EXTERNAL-FORMAT*
                                 using (hash-value value)
                                 when (eq value ef)
                                 return key))|#

(%initialize-ffc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  LIBRARIES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We rely on the built-in global
;;;  sb-sys:*shared-objects* ::=
;;;   (#S(SB-ALIEN::SHARED-OBJECT :pathname #P"USER32.DLL" :namestring "USER32.DLL"
;;;                               :handle 2011234304 :dont-save NIL) ...)
;;; On Windows, sb-alien:load-shared-object automatically returns existing connection.

;(defvar *loaded-modules-alist* ())

(defun load-foreign-module (name &key (real-name (module-real-name name))
                                      (lifetime :indefinite))
 ;;; Args: lifetime ::= :indefinite or :session
  ;; Q: On non-Windows platforms calling sb-alien:load-shared-object again
  ;;    with the same pathname argument will replace the existing definition.
  ;;    Shall we ensure the previous object to stays on?
  (sb-alien:load-shared-object real-name :dont-save (eq lifetime :session))
  name)
  ;(setf (cdr-assoc module *loaded-modules-alist* :test #'equalp) real-name)

(defun unload-foreign-module (module &key (real-name (module-real-name module))
                                          (remove t))
  (declare (ignore remove))
  (sb-alien:unload-shared-object real-name)
  t)
  ;(removef *loaded-modules-alist* module :key #'car :test #'equalp))

(defun module-loaded-pathname (module &key (real-name (module-real-name module)))
 ;;; Value: Pathname (not truename - only file name and extension on SBCL for Windows).
  ;;        or NIL if not connected.
  ;; Q: Check SHARED-OBJECT-DONT-SAVE and SHARED-OBJECT-HANDLE?
  (let ((object (cond ((pathnamep module)
                       (find module sb-sys:*shared-objects*
                             :key #'SB-ALIEN::SHARED-OBJECT-PATHNAME
                             :test #'equalp))			;pathname-equal
                      ((pathnamep real-name)
                       (find real-name sb-sys:*shared-objects*
                             :key #'SB-ALIEN::SHARED-OBJECT-PATHNAME
                             :test #'equalp))
                      (t (find module sb-sys:*shared-objects*
                               :key #'SB-ALIEN::SHARED-OBJECT-NAMESTRING
                               :test #'string-equal)))))
    (when object
      (SB-ALIEN::SHARED-OBJECT-PATHNAME object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TYPES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-type (name type)
 ;;; SBCL NB: name can be NIL for anonymous type, e.g.
  ;;		(define-alien-type nil (struct foo (a int) (b (array int 100))))
  `(sb-alien:define-alien-type ,name ,type))

;;; Definitions of canonical foreign types

(define-foreign-type :pointer (* t))
;(define-foreign-type :void sb-alien:void)	; signals: cannot use values types here

(define-foreign-type WORD (sb-alien:unsigned 16))
#-win32 (define-foreign-type DWORD (sb-alien:unsigned 32))

;;; BAD: Dereferencing pointer to an object of these character types results in
;;;      - character on LispWorks,
;;;      - integer char-code on SBCL.

(define-foreign-type :char    sb-alien:char)
(define-foreign-type :wchar-t (sb-alien:signed 16))	;sb-alien:short?

(define-foreign-type :boolean (boolean 32))

;(define-foreign-type :byte (sb-alien:signed 8))
(define-foreign-type :signed-byte (sb-alien:signed 8))
(define-foreign-type :unsigned-byte (sb-alien:unsigned 8))

;(define-foreign-type :short (sb-alien:signed 16))
(define-foreign-type :signed-short (sb-alien:signed 16))
(define-foreign-type :unsigned-short (sb-alien:unsigned 16))

;(define-foreign-type :long (sb-alien:signed 32))
(define-foreign-type :signed-long (sb-alien:signed 32))     ;#+x86-64 (sb-alien:signed 64)
(define-foreign-type :unsigned-long (sb-alien:unsigned 32)) ;#+x86-64(sb-alien:unsigned 64)
      
(define-foreign-type :float  single-float)
(define-foreign-type :double double-float)
;(define-foreign-type string-ptr sb-alien:c-string)

(eval-when (:compile-toplevel :load-toplevel)
(defun %sbcl-type (type &optional context)
  (if (atom type)
      ;(if (eq context :type)
      ;    (or (gethash type *cmu-def-type-hash*) (basic-convert-from-uffi-type type)))
      (case type
        (:void     'sb-alien:void)				; function result type
        (otherwise type))
      (case (first type)
	(cl:quote
	 (%sbcl-type (second type) context))
	(:pointer
	 (%sbcl-type `(* ,(second type)) context))
	(:boolean						; convert to bits
	 `(boolean ,(case (second type)				; encapsulates
                      (:byte 8)
                      (:short 16)
                      (:long 32)
                      (otherwise (second type)))))
	(:c-array
	 `(array ,(%sbcl-type (second type) context) ,@(cddr type)))
	;(:self							; (:self name)?
        ; `(* (,(case context
        ;         (:struct 'sb-alien:struct))
        ;      ,(%sbcl-type (second type) context))))
	;(:struct-pointer
	; (%sbcl-type `(* ,(second type)) :struct))
	;(:struct
	; (%sbcl-type (second type) :struct))
        ;(:union
	; (%sbcl-type (second type) :union))
        (otherwise
         (cons (%sbcl-type (first type) context) 
               (%sbcl-type (rest type) context))))))

(defun %ef-type (external-format)
 ;;; Foreign character type specifier for the EXTERNAL-FORMAT given
  (case external-format
    (#+little-endian (:ucs-2le :utf-16le)	; wchar-t
     #+big-endian    (:ucs-2be :utf-16be)	; no EF translation needed
     :wchar-t)
    (otherwise
     :char)))
)

(defun %convert-descriptions (args &optional context)
 ;;; Args: args List of slot or parameter descriptions.
  (loop for (name type) in args
        collect (list name (%sbcl-type type context))))

(defun %size-of (type)
 ;;; Value: The size in octets of a foreign type.
  (ash (sb-alien-internals:alien-type-bits  ;(convert-foreign-type type-keyword)
        (sb-alien-internals:parse-alien-type (%sbcl-type type) nil))
       -3))						; convert from bits to bytes
  ;(eval `(sb-alien:alien-size ,type :bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  POINTERS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Q: Is it possible to default offset for incf and decf?

(defconstant *null-pointer* (if (boundp '*null-pointer*) (symbol-value '*null-pointer*)
                                (sb-alien:sap-alien (sb-sys:int-sap 0) (* t))))

(declaim (inline %pointerp))
(defun %pointerp (pointer)
  (sb-alien-internals:alien-value-p pointer))

(declaim (inline %null-pointer-p))
(defun %null-pointer-p (pointer)
  (or (null pointer) (sb-alien:null-alien pointer)))	; signals on non-alien value

(declaim (inline %pointer-eql))
(defun %pointer-eql (x y)
  (sb-sys:sap= (sb-alien:alien-sap x) (sb-alien:alien-sap y)))

(defun %pointer-element-size (pointer)
  (- (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr (sb-alien:deref pointer 1))))
     (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr (sb-alien:deref pointer 0))))))

(defmacro %pointer-address (pointer)
 `(sb-sys:sap-int (sb-alien:alien-sap ,pointer)))

(defmacro %copy-pointer (pointer &optional offset)
 ;;; In-heap copy of the pointer given
  `(sb-alien:addr (sb-alien:deref ,pointer ,@(when offset `(,offset)))))
   ;(sb-kernel::copy-alien-value pointer)

;;; Increase or decrease the address held by the pointer.
;;; Args: offset When not given, the address is changed by the size of the type
;;;              pointed to by the pointer.
;;;              When given, the address is changed by a multiple of the size.
;;; Value: pointer.

(defmacro %incf-pointer (pointer &optional offset)
  (sb-int:with-unique-names (ptr)
    `(let ((,ptr ,pointer))
       (setf (sb-alien-internals:alien-value-sap ,ptr)
             (sb-alien:alien-sap (sb-alien:addr (sb-alien:deref ,ptr ,(or offset 1)))))
       ,ptr)))

(defmacro %decf-pointer (pointer &optional offset)
  (sb-int:with-unique-names (ptr)
    `(let ((,ptr ,pointer))
       (setf (sb-alien-internals:alien-value-sap ,ptr)
             (sb-alien:alien-sap (sb-alien:addr (sb-alien:deref ,ptr (- ,(or offset 1))))))
       ,ptr)))

(defmacro with-pointer ((ptr &key index type) pointer &body body)
 ;;; Bind PTR to in-stack copy of POINTER, increase by INDEX in needed, 
  ;; and evaluate the BODY. Should not cons!
  ;; Args: index  Index within a coerced pointer, evaluated.
  ;;       type   Element type, not evaluated.
  ;; SBCL vs LW:
  ;;    sb-alien:cast requires pointer-type, not element type!
  ;; Q: Does (declare (dynamic-extent ptr)) really help allocating ptr in stack?
 `(let ((,ptr (sb-alien:addr (sb-alien:deref ,(if type
                                               `(sb-alien:cast ,pointer
                                                               (* ,(%sbcl-type type)))
                                               pointer)
                                             ,@(when index `(,index))))))
    (declare (dynamic-extent ,ptr))
    ,@body))


;;; Allocation, deallocation, and dereference

(defmacro %alloc (type &optional nelems initial-element)
 ;;; Generaly used to allocate a static (in-heap) FLI object of the scalar type given.
  ;; Args: type            Not evaluated
  ;;       initial-element Only allowed if type is scalar, evaluated.
  ;; NB: When both the nelems and initial-element are specified, we should rebind nelems.
  (let* ((n nelems)
         (pair (if (and nelems initial-element)
                   `((,(setq n (gensym)) ,nelems))
                   nil))
         (form (if nelems
                  `(sb-alien:make-alien ,(%sbcl-type type) ,n)
                  `(sb-alien:make-alien ,(%sbcl-type type)))))
    (if initial-element
        (sb-int:with-unique-names (pointer)
           `(let* (,@pair
                   (,pointer ,form))
              ,(if nelems
                   (sb-int:with-unique-names (code i)
                     `(let ((,code ,initial-element))
                        (dotimes (,i ,n)
                          (declare (fixnum ,i))
                          (setf (sb-alien:deref ,pointer ,i) ,code))))
                   `(setf (sb-alien:deref ,pointer) ,initial-element))
              ,pointer))
        form)))

(declaim (inline %free))
(defun %free (pointer)
 ;;; Args: pointer  If *null-pointer* is passed, the function must do nothing.
  (sb-alien:free-alien pointer))

(defmacro %deref (pointer &optional index)
  `(sb-alien:deref ,pointer ,@(when index `(,index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  VECTOR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %alloc-vector (type nelems)
 ;;; Args: type  Element type, not evaluated
  ;; Value: Pointer to array, not scalar.
  ;; HACK: When nelems is determined at run-time, replace the dimensions within
  ;;       sb-alien-internals:alien-array-type sturcture object.
  ;;       (Shall we make a copy by means of sb-alien::copy-alien-array-type?)
  (let ((%sbcl-type (%sbcl-type type)))
    (if (constantp nelems)
        `(sb-alien:make-alien (array ,%sbcl-type ,(eval nelems)))
        (sb-int:with-unique-names (length ptr)
          `(let* ((,length ,nelems)
                  (,ptr (sb-alien:cast (sb-alien:make-alien ,%sbcl-type ,length)
                                       (* (array ,%sbcl-type)))))
             (setf (sb-alien-internals:alien-array-type-dimensions	; set dimention
                    (sb-alien-internals:alien-pointer-type-to
                     (sb-alien-internals:alien-value-type ,ptr)))	; to retrieve later
                   (list ,length))					; with %length
             ,ptr)))))

(defun ffc:%length (pointer)
  (first (sb-alien-internals:alien-array-type-dimensions
          (sb-alien-internals:alien-pointer-type-to
           (sb-alien-internals:alien-value-type pointer)))))

(defmacro %svref (array index &key type)
 ;;; Args: type  Element-type, not evaluated and must produce a scalar type.
  ;;		 If supplied, the array pointer is casted to this type first.
  ;;       index Subscript within the given or casted array.
  (if type
      `(sb-alien:deref (sb-alien:cast (sb-alien:deref ,array 0) (* ,(%sbcl-type type)))
                       ,index)
      `(sb-alien:deref (sb-alien:deref ,array 0) ,index)))

(defmacro %svref-pointer (array index)
 ;;; Pointer to the specified element
  `(sb-alien:addr (sb-alien:deref (sb-alien:deref ,array 0) ,index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRUCTURE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-struct (name &rest slots)
 `(sb-alien:define-alien-type ,name
    (sb-alien:struct ,name ,@(%convert-descriptions slots :struct))))

(defmacro %slot (pointer structure-type slot &key type copy-foreign-object)
 ;;; Args: pointer        Evaluated.
  ;;       structure-type Type of structure.
  ;;       slot           Symbol.
  ;;       type           Type of the resulting value.
  ;;       structure-type, slot, type are not evaluated.
  (declare (ignore type copy-foreign-object))
  `(sb-alien:slot ,(if structure-type
                       `(sb-alien:cast ,pointer (* ,structure-type))
                       pointer)
                  ',slot))

(defmacro %slot-pointer (pointer structure-type slot &key type)
 ;;; Args: pointer, copy-foreign-object - Evaluated
  ;;       structure-type, slot, type   - Are not evaluated.
  (declare (ignore type))
  `(sb-alien:addr (%slot ,pointer ,structure-type ,slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ELEMENTARY TYPES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro %get-octet (pointer index)
  `(sb-alien:deref (sb-alien:cast ,pointer (* (sb-alien:unsigned 8))) ,index))

(defmacro %get-word (pointer &optional index)
  `(sb-alien:deref (sb-alien:cast ,pointer (* (sb-alien:unsigned 16)))
                   ,@(when index `(,index))))

(defmacro %get-dword (pointer &optional index)
  `(sb-alien:deref (sb-alien:cast ,pointer (* (sb-alien:unsigned 32)))
                   ,@(when index `(,index))))

(declaim (ftype (function ((sb-alien:alien (* t)) symbol fixnum &optional fixnum) t)
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
  ;; caught WARNING:
  ;;   cannot cast to alien type
  ;;    #<SB-ALIEN-INTERNALS:ALIEN-INTEGER-TYPE (sb-alien:unsigned 8)>
  (declare (type (sb-alien:alien (* t)) pointer)
           (fixnum octet-length index)
           #-debug (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  ;(with-pointer (ptr :type :unsigned-byte) pointer
  (let ((sap (sb-alien:alien-sap pointer)))
    (declare (type sb-sys:system-area-pointer sap))
    (case result-type
      ((bit-vector simple-bit-vector)
       ;; For readability, MS bit of every octet is put on the left,
       ;; i.e. with a smaller subscript.
       ;; Старший бит из каждого байта помещается левее, т.е. с меньшим индексом
       ;; в битовом векторе. Как следствие, печатное представление вектора нагляднее.
       ;(%incf-pointer ptr index)
       (loop with vector = (make-array (ash octet-length 3) :element-type 'bit)
             for i of-type fixnum upfrom 0 below octet-length
             for byte of-type (unsigned-byte 8) = (sb-sys:sap-ref-8 sap index)
             ;for byte of-type fixnum = (%deref ptr)
             do (dotimes (j 8)
                  (declare (fixnum j))
                  (setf (sbit vector (+ (ash i 3) j)) (logand (ash byte (- j 7)) 1)))
                (incf index)
                ;(%incf-pointer ptr)
             finally (return vector)))
      (string 							; a la object-hex-string
       ;(%incf-pointer ptr index)
       (loop with string = (make-string (* octet-length 2) :element-type 'base-char)
             repeat octet-length
             for j of-type fixnum upfrom 0 by 2
             for byte of-type (unsigned-byte 8) = (sb-sys:sap-ref-8 sap index)
             ;for byte of-type fixnum = (%deref ptr)
             do (setf (schar string j) (digit-char (ash byte -4) 16)
                      (schar string (1+ j)) (digit-char (logand byte #x0F) 16))
                (incf index)
                ;(%incf-pointer ptr)
             finally (return string)))
      ((integer :bookmark)					; used in YstokSQL
       ;; Assuming little-endian, i.e. when the less significant byte goes first;
       ;; we can get either a fixnum or bignum as a result.
       (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         ;(%incf-pointer ptr index)
         (loop with n of-type integer = 0			; unsigned
               for byte-position of-type fixnum upfrom 0 by 8
               repeat octet-length
               do (setq n (dpb (sb-sys:sap-ref-8 sap index) ;(%deref ptr)
                               (byte 8 byte-position) n))
                  (incf index)
                  ;(%incf-pointer ptr)
               finally (return n))))
      (otherwise						; vector = octer-vector
       (let ((vector (make-array octet-length :element-type '(unsigned-byte 8))))
         (declare (type (simple-array (unsigned-byte 8) (*)) vector))
         (sb-kernel:copy-ub8-from-system-area sap index  vector 0  octet-length)
         vector)) )))

(declaim (ftype (function ((sb-alien:alien (* t)) t fixnum
                           &key (:start fixnum) (:index fixnum))
                          fixnum)
                %put-binary))
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
  ;; MISNOMER: sb-kernel:copy-ub8-to-system-area vs. sb-kernel:copy-ub8-from-system-area
  ;;           Both the name and the order of parameters differ. Brr..
  (declare (type (sb-alien:alien (* t)) pointer)
           (fixnum max-octet-length start index)
           #-debug (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (let ((sap (sb-alien:alien-sap pointer)))
    (declare (type sb-sys:system-area-pointer sap)) ;(dynamic-extent sap))
    ;(with-pointer (ptr :type :unsigned-byte :index index) pointer
    (cond ((bit-vector-p data)		
           #+debug (assert (zerop start) (start)
                     "Bit-vector can only be put into BINARY buffer without chunking.")
           (let* ((bit-count (length data))
                  (octet-length (ceiling bit-count 8)))
             (declare (fixnum bit-count octet-length))
             (loop for i of-type fixnum upfrom 0 below octet-length
                   and byte of-type (unsigned-byte 8) = 0
                   do (loop for j of-type fixnum upfrom 0 below 8
                            for index of-type fixnum = (+ (ash i 3) j)
                            while (< index bit-count)
                            do (setf byte (logior byte (ash (sbit data index) (- 7 j)))))
                      ;(setf (%deref ptr) byte)
                      ;(%incf-pointer ptr))
                      (setf (sb-sys:sap-ref-8 sap index) byte)
                      (incf index))
             octet-length))
          ((and (vectorp data)						; octet-vector-p
                (equal (array-element-type data) '(unsigned-byte 8)))
           (let ((octet-length (the fixnum (min (the fixnum (- (length data) start))
                                                max-octet-length))))
             (declare (fixnum octet-length))
             (if (< 0 octet-length)
                 (progn (sb-kernel:copy-ub8-to-system-area data start  sap 0  octet-length)
                   octet-length)
                 0)))
          ((integerp data)					; bookmark in YstokSQL
           ;; We fill all the reserved space with trailing zeros because its length
           ;; cannot be addapted to the integer length.
           ;(assert (zerop start) (start)
           ;  "Bookmarks can only be put into BINARY buffer without chunking.")
           (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
             (loop repeat max-octet-length
                   for byte-position of-type fixnum upfrom 0 by 8
                   do (setf (sb-sys:sap-ref-8 sap index) (ldb (byte 8 byte-position) data))
                      (incf index)))
                   ;do (setf (%deref ptr) (ldb (byte 8 byte-position) data))
                   ;   (%incf-pointer ptr))
           max-octet-length)
          (t (error "%PUT-BINARY: The type of DATA is not supported.")))))

(defmacro %fill-binary (pointer octet-length &optional (byte 0))
 ;;; Fill OCTET-LENGTH octets in foreign memory area POINTER with BYTE.
  ;(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  `(sb-kernel:system-area-ub8-fill ,byte (sb-alien:alien-sap ,pointer) 0 ,octet-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  STRING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Args: pointer Of type :char or :wchar-t, i.e. (signed 8) or (signed 16) on SBCL.
;;;       type ::= :char | :wchar-t | SBCL-native-integer-type - not evaluated
;;; Value/char: Lisp character object.

(defmacro %get-char (pointer &optional index type)
  `(code-char (sb-alien:deref ,(if type `(sb-alien:cast ,pointer (* ,type)) pointer)
                              ,@(when index `(,index)))))
(defmacro %put-char (pointer char &optional index type)
  `(setf (sb-alien:deref ,(if type `(sb-alien:cast ,pointer (* ,type)) pointer)
                         ,@(when index `(,index)))
         (char-code ,char)))

(declaim (ftype (function ((sb-alien:alien (* t)) &optional t) fixnum)
                %cstring-length))
(defun %cstring-length (pointer &optional (external-format *foreign-external-format*))
 ;;; Args: pointer         Null-terminated C string.
  ;;       external-format :UCS-2xx, :UTF-16xx, or whatever except for :UTF-8.
  ;; Value: Number of characters not counting #\Null terminator.
  ;; Q: Do we need this on SBCL at all?
  (declare (type (sb-alien:alien (* t)) pointer)
           #-debug (optimize (speed 3) (safety 0) (debug 0))
           #+debug (optimize (debug 3)))
  #+debug (assert (not (eq external-format :utf-8)))
  (let ((sap (sb-alien:alien-sap pointer)))
    (declare (type sb-sys:system-area-pointer sap)) ;(dynamic-extent sap))
    (case external-format
      (#+little-endian (:ucs-2le :utf-16le)
       #+big-endian    (:ucs-2be :utf-16be)
       (loop for offset of-type fixnum upfrom 0 by 2
             and i of-type fixnum upfrom 0
             until (zerop (sb-sys:sap-ref-16 sap offset))
             finally (return i)))
      (otherwise
       (loop for offset of-type fixnum upfrom 0
             until (zerop (sb-sys:sap-ref-8 sap offset))
             finally (return offset))) )))

(declaim (ftype (function ((or null string) &key (:external-format t) (:null-terminate t))
                          (sb-alien:alien (* t)))
                %lisp-cstring))
(defun %lisp-cstring (string &key (external-format *foreign-external-format*)
                                  (null-terminate t))
 ;;; Copy string into a null-terminated C-string freshly allocated in heap.
  ;; Args: string          Input Lisp string or NIL.
  ;;       external-format :UCS-2xx, :UTF-16xx, or whatever including :UTF-8.
  ;;                       E.g. :cp1251 or so on Windows.
  ;; Value: 1) pointer of foreign type (* (SIGNED 8)) or (* (SIGNED 16))
  ;;           to freshly allocated memomy
  ;;           the memory needs to be freed with %free.
  ;;        2) octet-length (actually NIL - bug?)
  ;; NB: Even for :external-format :UCS-2LE, sb-alien:make-alien-string actually returns
  ;;       #<SB-ALIEN-INTERNALS:ALIEN-VALUE :SAP # :TYPE (* (SIGNED 8))>
  ;; NB: Sometimes the Lisp string is not needed to be copied if its element-type matches
  ;;     the external-format specifed (SBCL Manual, 8.2.3 Foreign Type Specifiers).
  ;;     But this function is always makes a copy.
  (declare (type (or null string) string)
           #-debug (optimize (speed 3) (safety 0) (debug 0))
           #+debug (optimize (debug 3)))
  (if string
      (case external-format
        (#+little-endian (:ucs-2le :utf-16le)
         #+big-endian    (:ucs-2be :utf-16be)
         (sb-alien:cast #1=(sb-alien:make-alien-string string
                                                       :external-format external-format
                                                       :null-terminate null-terminate)
                        (* (sb-alien:signed 16))))
        (otherwise
         #1#))
      *null-pointer*))

(declaim (ftype (function ((sb-alien:alien (* t)) &key (:length t) (:external-format t))
                          (or null string))
                %cstring-lisp))
(defun %cstring-lisp (pointer &key length (external-format *foreign-external-format*))
 ;;; Create new Lisp string with the element-type according to external format
  ;; Args: pointer         Pointer to C-string (of an arbitrary type).
  ;;       length          Fixnum or FLI pointer to an integer value
  ;;                       - in characters for single-byte or two-byte ef like UCS-2,
  ;;                       - in octets for UTF-8 (where is it passed from for UTF-8?)
  ;;                       or NIL if the poitner refers to a null-terminated buffer.
  ;;       external-format :UCS-2xx, :UTF-16xx, or whatever including :UTF-8.
  (declare (type (sb-alien:alien (* t)) pointer)	; = sb-alien-internals:alien-value?
           #-debug (optimize (speed 3) (safety 0) (debug 0))
           #+debug (optimize (debug 3)))
  (cond ((%null-pointer-p pointer)
         nil)
        (length
         (let ((length (cond ((numberp length) length)
                             ((%pointerp length) (%deref length))
                             (t 0))) 				; should we signal instead?
               (sap (sb-alien:alien-sap pointer)))
           (declare (type sb-sys:system-area-pointer sap) ;(dynamic-extent sap)
                    (type fixnum length))
           (case external-format
             (#+little-endian (:ucs-2le :utf-16le)		; wchar-t
              #+big-endian    (:ucs-2be :utf-16be)		; no EF translation needed
              (let ((str (make-string length :element-type 'character)))
                (declare (type simple-string str))
                (loop for offset of-type fixnum upfrom 0 by 2
                      and i of-type fixnum upfrom 0 below length
                      do (setf (schar str i) (code-char (sb-sys:sap-ref-16 sap offset))))
                str))
             (:ascii
              (let ((str (make-string length :element-type 'base-char)))
                (declare (type simple-base-string str))
                (sb-kernel:copy-ub8-from-system-area sap 0 str 0 length)
                str))
             (:latin-1
              (let ((str (make-string length :element-type 'character)))
                (declare (type simple-string str))
                (loop for offset of-type fixnum upfrom 0 below length
                      do (setf (schar str offset)		; ef:find-external-char?
                               (code-char (sb-sys:sap-ref-8 sap offset))))
                str))
             (otherwise
              ;; length is an octet-length here.
              ;; BAD: SBCL has no means to specify the upper bound for c-string-to-string.
              ;;      How to optimize?
              (sb-ext:octets-to-string (%get-binary pointer 'vector length) ; octet-vector
                                       :external-format external-format)) )))
        (t
         (sb-alien::c-string-to-string (sb-alien:alien-sap pointer)
                                       external-format
                                       (if (eq external-format :ascii)
                                           'base-char
                                           'character))) ))

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
  ;;               Implementation specific e.g. :ucs-2le;
  ;;               defaults to *foreign-external-format*.
  ;;       null-terminate
  ;;               If false, C-string will not be terminated by the #\Null char.
  ;;               Terminating may be not needed if an explicit length is also
  ;;               passed to the foreign routine. 
  ;;               Evaluated at compile-time!
  (if (and (constantp string) (constantp external-format))
      (let ((len (length string))				; allocate in stack
            (type (%ef-type external-format)))
        (sb-int:with-unique-names (ptr i)
          `(sb-alien:with-alien ((,ptr (array ,type ,#1=(if null-terminate (1+ len) len))))
             ;(symbol-macrolet ((,pointer (sb-alien:cast ,ptr (* ,type))))
             (let ((,pointer (sb-alien:addr (sb-alien:deref ,ptr 0)))
                   ,@(when length
                       `((,length ,#1#)))
                   ,@(when octet-length
                       `((,octet-length ,(if (eq type :wchar-t) (ash #1# 1) #1#)))))
               (declare (dynamic-extent ,pointer))
               #+debug (assert (member ,external-format '(:ascii :latin-1)))
               (dotimes (,i ,len)				; initialize
                 (declare (fixnum ,i))				; TODO:
                 (%put-char ,pointer (schar ,string ,i) ,i))	;  ef:char-external-code
               ,@(when null-terminate
                   `((setf (%deref ,pointer ,len) 0)))
               ,@body))))
      (sb-int:with-unique-names (str) ;octets				; allocate in-heap
        ;; TODO: Allocate octets with dynamic-extent:
        ;;       - via (sb-alien::string-to-c-string string external-format)?
        ;;       - maybe fill it from the string,
        ;;       - cast to (* ,(%ef-type external-format))
        `(let* ((,str ,string)
                (,pointer (%lisp-cstring ,str			; possibly *null-pointer*
                                         ,:external-format ,external-format
                                         ,:null-terminate ,null-terminate)))
              #|(,octets (string-to-octets ,str :external-format ,external-format
                                                  ,:null-terminate ,null-terminate))
                (,pointer (sb-alien:sap-alien (sb-sys:vector-sap ,octets)
                                              (* ,*foreign-char-type*))))|#
           (declare (type sb-alien-internals:alien-value ,pointer)
                    (dynamic-extent ,pointer))
           ; (type simple-octet-vector ,octets) (dynamic-extent ,octets)
           (unwind-protect
               (let* (,@(when length
                          `((,length ,(if null-terminate
                                          `(if ,str (1+ (length ,str)) 0)
                                          `(length ,str)))))
                      ,@(when octet-length
                          `((,octet-length (if (sb-kernel:base-string-p ,str)
                                               ,length
                                               (ash ,length 1))))) )
                 ,@body)
             (%free ,pointer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CONVENIENCE MACROS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-foreign-function (name params result-type
                                   &key (module *default-foreign-module*)
                                        encoding
                                        inline
                                   &allow-other-keys)
 ;;; Args: calling-convention Ignored as SBCL seems to determine it automaticall
  (let* ((lisp-name (cond ((consp name) (first name))
                          ((stringp name) (intern (string-upcase name))) ; YSQL-> *package*
                          (t name)))
         (c-name (concatenate 'string
                              (cond ((consp name) (second name))
                                    ((stringp name) name)
                                    (t (symbol-name name)))
                              (case encoding
                                (:dbcs "W")	; assume 'A' suffex (Win95) no longer used
                                (otherwise ""))))
         (form `(sb-alien:define-alien-routine (,c-name ,lisp-name)
                                               ,(%sbcl-type result-type)
                 ,@(%convert-descriptions params))))		; assumed :in by default
    (if (or inline module)
        `(progn
           ,@(when inline `((declaim (inline ,lisp-name))))
           ,@(when module `((load-foreign-module ,module)))
           ,form)
        form)))

(defmacro define-foreign-variable (name type &key (accessor :value) module
                                             &allow-other-keys)
 ;;; Args: name ::= lisp-name | (lisp-name c-name-string)
  ;;       type Canonical foreign type, not evaluated.
  ;;       accessor ::= :address-of | :value
  ;; The lisp-name is effectively global symbol macro.
  ;; - a reference to the symbol defined with define-alien-variable fetches
  ;;   the contents of the external variable.
  ;; - setting such a variable stores new contents, which must be of the declared type.
  ;; Q: Maybe SBCL atomatically uses :accessor :value for scalar types?
  (let ((lisp-name (cond ((consp name) (first name))
                         ((stringp name) (intern (string-upcase name))) ; YSQL-> *package*
                         (t name)))
        (c-name (cond ((consp name) (second name))
                      ((stringp name) name)
                      (t (symbol-name name)))))
    `(progn
       ,@(when module `((load-foreign-module ,module)))
       ,(case accessor
          (:address-of
           ; `(define-symbol-macro ,lisp-name
           ;    (sb-alien:addr (sb-alien:extern-alien ,c-name ,(%sbcl-type type)))))
           (let ((var (gensym (string lisp-name))))
             `(progn (sb-alien:define-alien-variable (,c-name ,var) ,(%sbcl-type type))
                (define-symbol-macro ,lisp-name (sb-alien:addr ,var)))))
          (otherwise
           `(sb-alien:define-alien-variable (,c-name ,lisp-name) ,(%sbcl-type type))) ))))
          ;  (error "Foreign variable accessor ~s is not implemented." accessor))))))
            ;`((sb-alien:extern-alien ,c-name ,(%sbcl-type type))
            ;  (define-symbol-macro ,lisp-name ,c-name))

(defmacro with-foreign (bindings &body body)
 ;;; Allocate memory for foreign objects, execute the body, then deallocate the objects
  ;; Args: binding ::= (var foreign-type &key nelems initial-element initial-contents)
  ;;       foreign-type
  ;;         Not evaluated, one of
  ;;         - a designator of a defined foreign type, built-in or composite,
  ;;         - a pseudo-type symbol for converting by %lisp-cstring or %alloc-cstring:
  ;;           :c-string     - means not only base-string (in SBCL base-code-limit = 127)
  ;;                           but any single-byte encoding (:char),
  ;;           :ef-wc-string - means UCS-2 string (:wchar-t - double-byte encoding),
  ;;           string        - according to compilet-time value of *foreign-char-type*
  ;;                           and run-time value of *foreign-external-format*.
  ;;       nelems
  ;;         Evaluated, means allocating an area (not foreing vector!) of this length.
  ;;       initial-element
  ;;         Evaluated, as a rule used when nelems is not null and not for string types.
  ;;       initial-contents
  ;;         Evaluated, can only be specified when nelems is null.
  ;;         For string pseudo-types, must be a lisp string, not a character!
  ;; NB: In many cases, only one of :nelems or :initial-element is enough.
  ;;     For string types, exactly one of :initial-element on :nelems is allowed.
  ;; CAUTION:
  ;;   All the variables are considered to be bound sequentially (as with let*),
  ;;   simple type variables are bound before complex.
  ;;   On LispWorks, complex type variables are bound in parallel!
  ;; CAUTION:
  ;;   In with-alien, initial-value should be of pointer type, not value type!
  ;; BAD: SBCL does not allow in-stack allocation of size determined at run-time!
  ;; NB:  sb-alien:with-alien supports only &optional initial-value.
  ;;       foreign-type ::= (sb-alien:struct foo) - allowed anonymous
  (let ((simple ())						; -> (with-alien (...))
        ;(macros ())						; -> (symbol-macrolet ...)
        (complex ())						; -> (let () ...)
        (types ())						; ::= ((var . type) ...)
        (dynamic ())						; -> (dynamic-extent ...)
        (init ())					; -> (... ,@body)
        (protect ()))						; -> (%free ...)
    (dolist (binding bindings)
      (destructuring-bind (var type &key nelems initial-element
                                         initial-contents (null-terminate t supplied-p))
          binding
       (labels ((%both-error ()
                  (error "Both NELEMS and INITIAL-CONTENTS specified for type ~s."
                         type))
                (%string (type external-format)
                  ;; Args: type Foreign type in SBCL notation.
                  (cond ((null nelems)
                         ;; Should we try to allocate octet-vector?
                         (if initial-contents
                             (progn				; allocate in heap
		               ;#+debug (assert (stringp ,initial-contents))
                               (push `(,var (%lisp-cstring ,initial-contents
                                             ,@(when external-format
                                                 `(,:external-format ,external-format))
                                             ,@(when supplied-p
                                                 `(,:null-terminate ,null-terminate))))
                                     complex)
                               (push var protect))
                             (error
                              "Neither NELEMS nor INITIAL-CONTENTS specified for type ~s."
                              type)))
                        (initial-contents
                         (%both-error))
                        ((constantp nelems)			; allocate in stack
                         (sb-int:with-unique-names (ptr)
                           (push `(,ptr (array ,type ,(eval nelems)))
                                 simple)
                           ;(push `(,var (sb-alien:cast ,ptr (* ,type))) macros)
                           ;(push `(,var (sb-alien:cast ,ptr (* ,type))) complesx)
                           (push `(,var (sb-alien:addr (sb-alien:deref ,ptr 0)))
                                 complex)			; should point to an elt
                           (push var dynamic)
                           (when initial-element
                             (sb-int:with-unique-names (code i)
                               (push `(let ((,code (char-code ,initial-element)))
                                        (dotimes (,i ,(eval nelems))
                                          (declare (fixnum ,i))
                                          (setf (%deref ,var ,i) ,code)))
                                     init)))))
                        (t					; allocate in heap
                         (push `(,var (%alloc ,type ,nelems
                                              ,@(when initial-element
                                                  `((char-code ,initial-element)))))
                               complex)
                         (push var protect)))) )
        (case type
          (string						; use defaults
           (%string *foreign-char-type*				; compile-time value
                    '*foreign-external-format*))		; run-time value
          (:c-string
           (%string :char '*char-external-format*))
          #+sb-unicode
          (:ef-wc-string
           (%string :wchar-t #+little-endian :ucs-2le
                             #+big-endian :ucs-2be))
          (otherwise
	   ;; Non-character type - no external-format conversion
           (let ((%sbcl-type (%sbcl-type type)))
             (cond ((null nelems)				; pointer to a scalar
                    (sb-int:with-unique-names (ptr)
                      (push `(,ptr ,%sbcl-type
                                   ,@(cond (initial-contents `(,initial-contents))
                                           (initial-element `(,initial-element))))
                            simple)
                      (push `(,var (sb-alien:addr ,ptr))
                          complex)
                      (push var dynamic)))
                   (initial-contents
                     (%both-error))
                   ((constantp nelems)				; try to allocate in-stack
                    (if (and (consp type) (eq (car type) :c-array))
                        (progn (unless (rest %sbcl-type)	; nelems is dimension
                                 (setf (rest %sbcl-type) (list (eval nelems))))
                          (push `(,var ,%sbcl-type)		; should point to a vector
                                simple))
                          ;; TODO: Check for initial-element.
                          ;; Q: Where is (var (:c-array ..) :initial-element ..) used?
                        (sb-int:with-unique-names (ptr)
                          (push `(,ptr (array ,%sbcl-type ,(eval nelems)))
                                simple)
                          ;(push `(,var (sb-alien:cast ,ptr (* ,%sbcl-type))) macros)
                          ;(push `(,var (sb-alien:cast ,ptr (* ,%sbcl-type))) complex)
                          (push `(,var (sb-alien:addr (sb-alien:deref ,ptr 0)))
                                complex)			; should point to an elt
                          (push var dynamic)
                          (when initial-element		; initialize the vector
                            (sb-int:with-unique-names (code i)
                              (push `(let ((,code #7=,(if (member type '(:char :wchar-t))
                                                          `(char-code ,initial-element)
                                                          initial-element)))
                                       (dotimes (,i ,(eval nelems))
                                         (declare (fixnum ,i))
                                         (setf (%deref ,var ,i) ,code)))
                                    init))))))
                   ((eq type :unsigned-byte)			; allocate in stack
		    ;; Dynamic-extent optimization does works for vector
                    ;; but the pointer itself seems to be allocation in heap
                    (sb-int:with-unique-names (octets)
                      (push `(,octets (make-array (the fixnum ,nelems)
                                       :element-type '#8=(unsigned-byte 8)
                                       ,@(when initial-element
                                           `(,:initial-element ,initial-element))))
                            complex)
                      (push `(,octets . ,'(simple-array #8# (*))) types)
                      (push octets dynamic)
                      ;; This seems not needed for dynamic-extent
                      ;;   (declare (sb-kernel:simple-unboxed-array (*)) octets)
                      ;;   (sb-sys:with-pinned-objects (octets) ...)
                      ;; This would shrink heap allocation about twice due to SAP
                      ;; structures are smaller than ALIEN-VALUEs
                      ;;   (push `(,var (sb-sys:vector-sap ,octets)) complex)
                      ;;   (push `(,var . sb-sys:system-area-pointer) types)
		      ;: BAD: Does allocate in-stack. Maybe use macrolet?
                      (push `(,var (sb-alien:sap-alien (sb-sys:vector-sap ,octets)
                                                       (* :unsigned-byte)))
                            complex)
                      (push `(,var . sb-alien-internals:alien-value) types)
                      (push var dynamic)))
                   (t						; allocate in heap
                    (push `(,var (%alloc ,%sbcl-type ,nelems
                                         ,@(when initial-element `(#7#))))
                          complex)
                    (push var dynamic)				; helps 
                    (push var protect)))))
    ) )))
    (cond (simple
           `(sb-alien:with-alien ,(nreverse simple)
             ;(symbol-macrolet ,(nreverse macros)
             ,@(if complex
                   `(#9=(let* ,(nreverse complex)
                          (declare ,@(mapcar (lambda (pair) `(type ,(cdr pair),(car pair)))
                                             (nreverse types))
                                   ,@(when dynamic
                                       `((dynamic-extent ,@(nreverse dynamic)))))
                          ,@(if protect
                                `((unwind-protect (progn ,@(nreverse init) ,@body)
                                    ,@(loop for var in protect		; deallocate in
                                            collect `(%free ,var))))	; reverse order
                                `(,@(nreverse init) ,@body))))
                   `(,@(nreverse init) ,@body))))
           (complex
            `#9#)
           (t `(progn ,@body)) )))

#||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+unsued
(defmacro %address-pointer (address type)
  `(sb-alien:sap-alien (sb-sys:int-sap ,address) ,(%sbcl-type type)))

#+old
(defun %copy-foreign-memory (src-ptr dst-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (sb-kernel:system-area-ub8-copy src-ptr 0 dst-ptr 0 length))

#+unused
(defun %convert-struct-slots (name slots)
  (loop for (name type) in slots
        collect (list name
                      (case type
                        (:pointer      `(* t))
                        (:pointer-self `(* (sb-alien:struct ,name)))
                        (otherwise     type)))))


#+(and unused (not sb-unicode))
(defun sbcl-naturalize-cstring (sap &key length (null-terminated-p t))
  (declare (type sb-sys:system-area-pointer sap)
	   (type (or null fixnum) length))
           (optimize (speed 3) (safety 0)))
   (let ((null-terminated-length (when null-terminated-p
                                   (loop for offset of-type fixnum upfrom 0
                                         until (zerop (sb-sys:sap-ref-8 sap offset))
                                         finally (return offset)))))
     (if length
	 (when (and null-terminated-length
                    (< (the fixnum null-terminated-length) (the fixnum length)))
           (setq length null-terminated-length))
         (setq length null-terminated-length)))
   (let ((result (make-string length)))
     (funcall *system-copy-fn* sap 0 result +system-copy-offset+
              (* length +system-copy-multiplier+))
     result)))

#+(and unused sb-unicode)
(defun sbcl-naturalize-cstring (sap &key length (null-terminated-p t))
  (declare (type sb-sys:system-area-pointer sap)
	   (type (or null fixnum) length)
           (optimize (speed 3) (safety 0)))
   (if null-terminated-p
       (let ((casted (sb-alien:cast (sb-alien:sap-alien sap (* char))
                                    sb-alien:utf8-string)))
         (if length
             (copy-seq (subseq casted 0 length))
             (copy-seq casted)))
       (let ((result (make-string length)))         ;; this will not work in sb-unicode
         (funcall *system-copy-fn* sap 0 result +system-copy-offset+
                  (* length +system-copy-multiplier+))
         result)))

;;; YstokSQL/FFC

(defmacro %get-short (ptr) `(%deref ,ptr))
(defmacro %put-short (ptr short) `(setf (%get-ptr ,ptr) ,short))
(defmacro %get-bit (ptr) `(%deref ,ptr))
(defmacro %get-long (ptr)      `(%deref ,ptr))
(defmacro %put-long (ptr long) `(setf (%get-ptr ,ptr) ,long))
(defmacro %get-signed-word (ptr)    `(%deref ,ptr))
(defmacro %get-unsigned-long (ptr) `(%deref ,ptr))
(defmacro %get-signed-long (ptr)   `(%deref ,ptr))
(defmacro %get-single-float (ptr)  `(%deref ,ptr))
(defmacro %get-double-float (ptr)  `(%deref ,ptr))

(def-alien-type sql-handle (* t))
(def-alien-type sql-handle-ptr (* sql-handle))
;(def-alien-type sql-handle-ptr sql-handle)

(defun %get-cstring-length (ptr)
  (loop with size = 0
        until (char= (deref ptr size) #\Null)
        do (incf size)
        finally (return size)))

(defun %cstring-into-string (ptr string offset size-in-bytes)
  (let 
    (dotimes (i length)
      (setf (char string offset) (deref ptr i))
      (incf offset))
    offset)

(defun %cstring-into-vector (ptr vector offset size-in-bytes)
    (dotimes (i size-in-bytes)
      (setf (aref vector offset)
            (deref ptr i))
      (incf offset))
    offset)

(defmacro %with-sql-pointer ((pointer-var) &body body)
  `(let ((,pointer-var (make-alien sql-handle-ptr)))
     ,@body))

;;; Untyped SAP approach

(defconstant* *null-pointer* (sb-sys:int-sap 0))
(setf (fdefinition '%pointerp)    (fdefinition 'sb-sys:system-area-pointer-p))
(setf (fdefinition '%pointer-eql) (fdefinition 'sb-sys:sap=))
(defun %null-pointer-p (sap) (zerop (sb-sys:sap-int sap)))


;;; For in-stack allocation (%alloc ... :dynamic t) something like this
(LET ((#:VAR607 (SB-ALIEN-INTERNALS:MAKE-LOCAL-ALIEN ,type)))
                   ; '#<sb-alien-internals:local-alien-info (SIGNED 32)>)))
  (SB-ALIEN-INTERNALS:NOTE-LOCAL-ALIEN-TYPE
      '#<SB-ALIEN-INTERNALS:LOCAL-ALIEN-INFO (SIGNED 32)> #:VAR607)
    (SYMBOL-MACROLET ((V (SB-ALIEN-INTERNALS:LOCAL-ALIEN
                           '#<SB-ALIEN-INTERNALS:LOCAL-ALIEN-INFO (SIGNED 32)>
                           #:VAR607)))
      (SETQ V ,initial-element)
      ...))

||#
