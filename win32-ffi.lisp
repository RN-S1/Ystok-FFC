;;; -*- Mode: Lisp; -*-
;;; Ystok Foreign Function Compatibility layer - Windows API bindings
;;; Copyright (c) 2003-2014 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants and FFC definitions of useful API routines.

(in-package :ystok.ffc)

(defparameter *default-win32api-module* "kernel32")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro define-win32api (name params result-type &rest args
                           &key (module '*default-win32api-module*)
                                encoding
                                export
                           &allow-other-keys)
  ;;; Args: name Symbol, its print-name must have the case of all letters preserved.
 `(progn
    (when ,export (export ',name))
    (define-foreign-function (,name ,(symbol-name name)) ,params ,result-type
      :calling-convention :stdcall
      :encoding ,encoding		 		; :source or :dbcs
      :module ,module
      #+lispworks ,@(system::remove-properties args '(:export :module :encoding))
      #-lispworks ,@(progn (remf args :export) (remf args :module) (remf args :encoding)
                      args)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  CHARACTER CASE CONVERSION  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Win32 API uses the language driver for the current language selected
;;; by the user at setup or by using Control Panel. 
;;; LW Q: Shall we use WIN32::LOCALE-CHAR-DOWNCASE$CHAR-CODE
;;;       and WIN32::LOCALE-CHAR-DOWNCASE$CHAR-CODE instead?

(define-win32api |CharLower| ((lpsz :signed-long)) :signed-long
  :encoding :dbcs :module "user32")
(define-win32api |CharUpper| ((lpsz :signed-long)) :signed-long
  :encoding :dbcs :module "user32")

(define-win32api |CharLowerBuff|
  ((lpsz :pointer)			; LPTSTR lpsz	  ; characters
   (length DWORD))			; DWORD cchLength ; number of TCHARs
  :void
  :encoding :dbcs :module "user32")
(define-win32api |CharUpperBuff|
  ((lpsz :pointer)
   (length DWORD))
  :void
  :encoding :dbcs :module "user32")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  GetLocaleInfo  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The function always retrieves information in text format.
;;; If the information is a numeric value, the function converts the number
;;; to text using decimal notation.
;;;
;;; The LOCALE_FONTSIGNATURE parameter will return a non-NULL terminated string.
;;; In all other cases, the string is NULL terminated.

;; For Locale parameter

(defconstant LOCALE_SYSTEM_DEFAULT #x800)
(defconstant LOCALE_USER_DEFAULT #x400)

;; For info-type parameter

#|
' LOCALE_ILANGUAGE = &H1         '  language id
' LOCALE_SLANGUAGE = &H2         '  localized name of language
' LOCALE_SENGLANGUAGE = &H1001   '  English name of language
' LOCALE_SABBREVLANGNAME = &H3   '  abbreviated language name
' LOCALE_SNATIVELANGNAME = &H4   '  native name of language
' LOCALE_ICOUNTRY = &H5          '  country code
' LOCALE_SCOUNTRY = &H6          '  localized name of country
' LOCALE_SENGCOUNTRY = &H1002    '  English name of country
' LOCALE_SABBREVCTRYNAME = &H7   '  abbreviated country name
' LOCALE_SNATIVECTRYNAME = &H8   '  native name of country
' LOCALE_IDEFAULTLANGUAGE = &H9  '  default language id
' LOCALE_IDEFAULTCOUNTRY = &HA   '  default country code
' LOCALE_IDEFAULTCODEPAGE = &HB  '  default code page
 |#
(defconstant LOCALE_SLIST	#xC)		; list item separator
; LOCALE_IMEASURE = &HD				' 0 = metric, 1 = US
(defconstant LOCALE_SDECIMAL	#xE)		; decimal separator
(defconstant LOCALE_STHOUSAND	#xF)		; thousand separator
;(defconstant LOCALE_SGROUPING	#x10)		; digit grouping
(defconstant LOCALE_IDIGITS	#x11)		; number of fractional digits
; LOCALE_ILZERO	#x12)				; leading zeros for decimal
; LOCALE_SNATIVEDIGITS = &H13			; native ascii 0-9
(defconstant LOCALE_SCURRENCY	#x14)		; local monetary symbol
; LOCALE_SINTLSYMBOL = &H15			; intl monetary symbol
(defconstant LOCALE_SMONDECIMALSEP #x16)	; monetary decimal separator
(defconstant LOCALE_SMONTHOUSANDSEP #x17)	; monetary thousand separator
;(defconstant LOCALE_SMONGROUPING #x18)		; monetary grouping
(defconstant LOCALE_ICURRDIGITS	#x19)		; local monetary digits
; LOCALE_IINTLCURRDIGITS = &H1A			; intl monetary digits
(defconstant LOCALE_ICURRENCY	#x1B)		; positive currency mode: 0,1,2,3
; LOCALE_INEGCURR = &H1C			; negative currency mode
(defconstant LOCALE_SDATE	#x1D)		; date separator
(defconstant LOCALE_STIME	#x1E)		; time separator
(defconstant LOCALE_SSHORTDATE	#x1F)		; short date format string
; LOCALE_SLONGDATE = &H20			; long date format string
(defconstant LOCALE_STIMEFORMAT	#x1003)		; time format string
(defconstant LOCALE_IDATE	#x21) ; short date format ordering: 0=MDY, 1=DMY, 2=YMD 
; LOCALE_ILDATE = &H22				; long date format ordering
(defconstant LOCALE_ITIME	#x23) ; time format specifier: 0=AM/PM 12-hour,1=24-hour
(defconstant LOCALE_ICENTURY	#x24) ; century format specifier: 0=2-digit, 1=4-digit

#|LOCALE_ITLZERO = &H25          '  leading zeros in time field
' LOCALE_IDAYLZERO = &H26        '  leading zeros in day field
' LOCALE_IMONLZERO = &H27        '  leading zeros in month field
' LOCALE_S1159 = &H28            '  AM designator
' LOCALE_S2359 = &H29            '  PM designator |#

(defconstant LOCALE_SDAYNAME1 	#x2A)		; long name for Monday
#|LOCALE_SDAYNAME2 = &H2B        '  long name for Tuesday
' LOCALE_SDAYNAME3 = &H2C        '  long name for Wednesday
' LOCALE_SDAYNAME4 = &H2D        '  long name for Thursday
' LOCALE_SDAYNAME5 = &H2E        '  long name for Friday
' LOCALE_SDAYNAME6 = &H2F        '  long name for Saturday
' LOCALE_SDAYNAME7 = &H30        '  long name for Sunday|#

(defconstant LOCALE_SABBREVDAYNAME1 #x31)	; abbreviated name for Monday
#|LOCALE_SABBREVDAYNAME2 = &H32  '  abbreviated name for Tuesday
' LOCALE_SABBREVDAYNAME3 = &H33  '  abbreviated name for Wednesday
' LOCALE_SABBREVDAYNAME4 = &H34  '  abbreviated name for Thursday
' LOCALE_SABBREVDAYNAME5 = &H35  '  abbreviated name for Friday
' LOCALE_SABBREVDAYNAME6 = &H36  '  abbreviated name for Saturday
' LOCALE_SABBREVDAYNAME7 = &H37  '  abbreviated name for Sunday |#

(defconstant LOCALE_SMONTHNAME1 #x38)		; long name for January
#|LOCALE_SMONTHNAME2 = &H39      '  long name for February
' LOCALE_SMONTHNAME3 = &H3A      '  long name for March
' LOCALE_SMONTHNAME4 = &H3B      '  long name for April
' LOCALE_SMONTHNAME5 = &H3C      '  long name for May
' LOCALE_SMONTHNAME6 = &H3D      '  long name for June
' LOCALE_SMONTHNAME7 = &H3E      '  long name for July
' LOCALE_SMONTHNAME8 = &H3F      '  long name for August
' LOCALE_SMONTHNAME9 = &H40      '  long name for September
' LOCALE_SMONTHNAME10 = &H41     '  long name for October
' LOCALE_SMONTHNAME11 = &H42     '  long name for November
' LOCALE_SMONTHNAME12 = &H43     '  long name for December |#

(defconstant LOCALE_SABBREVMONTHNAME1 #x44)	; abbreviated name for January
#|' LOCALE_SABBREVMONTHNAME2 = &H45 '  abbreviated name for February
' LOCALE_SABBREVMONTHNAME3 = &H46 '  abbreviated name for March
' LOCALE_SABBREVMONTHNAME4 = &H47 '  abbreviated name for April
' LOCALE_SABBREVMONTHNAME5 = &H48 '  abbreviated name for May
' LOCALE_SABBREVMONTHNAME6 = &H49 '  abbreviated name for June
' LOCALE_SABBREVMONTHNAME7 = &H4A '  abbreviated name for July
' LOCALE_SABBREVMONTHNAME8 = &H4B '  abbreviated name for August
' LOCALE_SABBREVMONTHNAME9 = &H4C '  abbreviated name for September
' LOCALE_SABBREVMONTHNAME10 = &H4D '  abbreviated name for October
' LOCALE_SABBREVMONTHNAME11 = &H4E '  abbreviated name for November
' LOCALE_SABBREVMONTHNAME12 = &H4F '  abbreviated name for December
' LOCALE_SABBREVMONTHNAME13 = &H100F |#

(define-win32api |GetLocaleInfo|
 ((Locale DWORD)
  (info-type :signed-long)
  (buffer :pointer)
  (length :signed-long))	; :constant 64
 :signed-long
 :encoding :dbcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FILE-UTILS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-win32api |GetFileTitle|
 ((lpszFile :pointer)
  (buffer :pointer)
  (length WORD))
 :signed-short
 :encoding :dbcs :module "comdlg32")


#||
#+lispworks
(WIN32:FORMAT-MESSAGE #x1000 nil 127 0 nil) ; FORMAT_MESSAGE_FROM_SYSTEM
(WIN32:GET-LAST-ERROR)
(WIN32:GET-LAST-ERROR-STRING)
(WIN32:GET-LAST-ERROR-STRING :error-code 5)
||#
