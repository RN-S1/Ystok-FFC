# Ystok-FFC

Foreign Function Compatibility layer


Ystok-FFC is a small portable Common Lisp library of wrapper functions and macros for the foreign function interface (FFI). The package provides a compatibility layer and allows the application code to call the native FFI functionality of various Common Lisp implementations in a uniform manner.

Ystok-FFC includes primitives for the following:
* defining foreign functions, structures, and scalar types,
* calling foreign functions,
* allocating and dereferencing foreign memory on low-level,
* loading shared libraries.

Design principles
-----------------

* "Canonical" specifiers of foreign types in Ystok-FFC are the same as in LispWorks. Syntax of some macros and signature of some functions were also borrowed from the LispWorks package fli.
* For many operations, Ystok-FFC provides shorter names than the LispWorks fli package.
* External format specifiers are not canonicalized. They are the same as defined by the underlying Common Lisp implementation, e.g. LispWorks or SBCL. UTF-8 is supported by all functions except <code>%cstring-length</code>, which is not exported.
* Though foreign calls are usual place to change character encoding, we recommend to avoide encoding and decoding at all. Everybody should select the foreign module version that relies on the external format that matches the internal string representation on the Common Lisp implementation.
* Void pointers are preferred in FFI declaration where foreign string are passed. This is due to  SBCL <code>sb-alien:make-alien-string</code> always returns <code># SB-ALIEN-INTERNALS:ALIEN-VALUE :SAP # :TYPE (* (SIGNED 8))</code> even for wide-character external formats like <code>:UCS-2LE</code>.
* For SBCL, we rely on SBCL typed pointers (of type alien-value) rather than untyped SAPs (in contrast to CFFI).


Limitations
-----------

Foreign callables (callbacks) are not supported by the current version.

Ystok-FFC does not pretend to cover all the foreign function interface features encountered in all modern Common Lisp implementations. It is only targeting at the most popular platforms and supports only the primitives that are really used in our projects.


Platforms
---------

LispWorks - Tested in full on versions 4.4 and 6.1 for Windows.
SBCL - Tested on version 1.0.55 on Windows.


The distribution package contains both the ASDlite/ASDF-based yffc.asd and the LispWorks-based defsys.lisp system definition files.


<i>Sources</i><br>
<i>[http://ystok.ru/](http://lisp.ystok.ru/yffc/)</i>
