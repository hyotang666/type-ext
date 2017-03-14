(defpackage :type-ext.spec (:use :cl :jingoh :type-ext))
(in-package :type-ext.spec)
(setup :type-ext.spec)

(requirements-about PROTOTYPE)

;;;; Description:
; C style function prototype declaration.
#?(prototype hoge (list)t)
:expanded-to (DECLAIM (FTYPE (FUNCTION (LIST)(VALUES T &OPTIONAL))
			     HOGE))

#+syntax
(PROTOTYPE name (&rest param-type*) return-type) ; => result

;;;; Arguments and Values:

; name := symbol which represents function name.
; Otherwise error.
; Not evaluated.
#?(prototype "CAR" (list)t) :signals type-error
,:lazy t

; param-type := type-specifier
; Otherwise unspecified.
; Not evaluated.
#?(prototype car (0)T) => unspecified

; return-type := type-specifier
; Otherwise unspecified.
; Not evaluated.
#?(prototype car (list)elt) => unspecified

; result := implementation-dependent
#?(prototype hoge (list)t) => implementation-dependent

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFINE-SIMPLE-TYPE)

;;;; Description:
; Short hand for defining new type and its predicates.
#?(define-simple-type(octet)
    '(unsigned-byte 8))
:expanded-to (EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
	       (DECLAIM(INLINE OCTET-P))
	       (DEFTYPE OCTET()
		 '(UNSIGNED-BYTE 8))
	       (DEFUN OCTET-P(ARG)
		 (TYPEP ARG 'OCTET))
	       '(OCTET OCTET-P))

#?(define-simple-type(symbols (:element-type symbol)
			      (:element-predicate symbolp)))
:expanded-to (EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
	       (DECLAIM(INLINE SYMBOLS-P))
	       (DEFUN SYMBOLS-P(ARG)
		 (VALUES(IGNORE-ERRORS(EVERY #'SYMBOLP ARG))))
	       (DEFTYPE SYMBOLS(&OPTIONAL ARG)
		 (IF (EQ '* ARG)
		   '(SATISFIES SYMBOLS-P)
		   (NESTED-CONS-TYPE-SPECIFIER 'SYMBOL ARG)))
	       '(SYMBOLS SYMBOLS-P SYMBOLP))

#+syntax
(DEFINE-SIMPLE-TYPE (name &rest option*) &body body) ; => result

;;;; Arguments and Values:

; name := symbol, otherwise error.
#?(define-simple-type("octet")
    '(unsigned-byte 8))
:signals type-error
,:lazy t

; option* := (option-key option-value)
; option-key := (member :predicate :element-type :element-predicate)
; option-value := see below

;;; option-values
;; :predicate option-value := symbol, otherwise error.
#?(define-simple-type(octet(:predicate "bad-example"))
    '(unsigned-byte 8))
:signals error

; :predicate specifies defined predicate name.
#?(define-simple-type(octet (:predicate |example|))
    '(unsigned-byte 8))
:expanded-to (EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
	       (DECLAIM(INLINE |example|))
	       (DEFTYPE OCTET()
		 '(UNSIGNED-BYTE 8))
	       (DEFUN |example|(ARG)
		 (TYPEP ARG 'OCTET))
	       '(OCTET |example|))

; Unless specified, predicate name is made automatically by concatenating specified typename with "-P".
; Remember CL:DEFSTRUCT.

;; :element-type option-value := type-specifier, otherwise unspecified.

; :element-type specifies list element type.
#?(define-simple-type(strings (:element-type string)))
:expanded-to(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
	       (DECLAIM(INLINE STRINGS-P))
	       (DEFUN STRINGS-P(ARG)
		 (VALUES(IGNORE-ERRORS(EVERY #'STRING-P ARG))))
	       (DEFTYPE STRINGS(&OPTIONAL ARG)
		 (IF (EQ '* ARG)
		   '(SATISFIES STRINGS-P)
		   (NESTED-CONS-TYPE-SPECIFIER 'STRING ARG)))
	       '(STRINGS STRINGS-P STRING-P))

;; :element-predicate option-value := function-designator, otherwise unspecified.
#?(define-simple-type(strings (:element-predicate :not-function-designator)
			      (:element-type string)))
=> unspecified

; :element-predicate specifies function designator which tests list element.
#?(define-simple-type(strings (:element-type string)
			      (:element-predicate stringp)))
:expanded-to(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
	       (DECLAIM(INLINE STRINGS-P))
	       (DEFUN STRINGS-P(ARG)
		 (VALUES(IGNORE-ERRORS(EVERY #'STRINGP ARG))))
	       (DEFTYPE STRINGS(&OPTIONAL ARG)
		 (IF (EQ '* ARG)
		   '(SATISFIES STRINGS-P)
		   (NESTED-CONS-TYPE-SPECIFIER 'STRING ARG)))
	       '(STRINGS STRINGS-P STRINGP))

; body := as CL:DEFTYPE body.

; result := list which contains new defined type name and its predicates.

;;;; Affected By:
; none

;;;; Side-Effects:
; Define new type and functions.

;;;; Notes:

;;;; Exceptional-Situations:
; when :element-predicate specified without :element-type, an error is signaled.
#?(define-simple-type(strings(:element-predicate stringp)))
:signals error

; when body is empty without :element-type, an error is signaled.
#?(define-simple-type(octet)) :signals error

; when :element-type is spcified with body, an error is signaled.
#?(define-simple-type(strings(:element-type string))
    '(satisfies strings-p))
:signals error

(requirements-about MAYBE)
;;;; Description:
; Short hand for (or null ...)
#?(typep nil '(maybe string)) => T
#?(typep "" '(maybe string)) => T
#?(typep T '(maybe string)) => NIL
;;;; Compound Type Specifier Kind:
; Specializing.

;;;; Compound Type Specifier Syntax:
; (maybe &optional type)
; When type is not specified, it represents T.
#?(typep nil '(maybe)) => T

;;;; Compound Type Specifier Arguments:
; type := type-specifier

;;;; Compound Type Specifier Description:

(requirements-about GENERALIZED-BOOLEAN)
;;;; Description:
; Denotes this type is used as boolean.
; When true, it is not specified T.
#?(typep T 'generalized-boolean) => T
#?(typep NIL 'generalized-boolean) => T
#?(typep 0 'generalized-boolean) => T

;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

(requirements-about NESTED-CONS-TYPE-SPECIFIER)

;;;; Description:
; Helper for make nested cons type specifier.
#?(nested-cons-type-specifier 'symbol 3)
=> (CONS SYMBOL (CONS SYMBOL (CONS SYMBOL NULL)))
,:test equal

#+syntax
(NESTED-CONS-TYPE-SPECIFIER type num) ; => result

;;;; Arguments and Values:

; type := type specifier.

; num := integer, otherwise error.
#?(nested-cons-type-specifier 'symbol 1)
=> (CONS SYMBOL NULL)
,:test equal
#?(nested-cons-type-specifier 'symbol 0)
=> NULL
#?(nested-cons-type-specifier 'symbol -1)
=> NULL
#?(nested-cons-type-specifier 'symbol :not-non-negative-integer)
:signals error

; result := type specifier.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

