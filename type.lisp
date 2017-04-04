(defpackage :type-ext(:use :cl)
  (:export
    ;;;; main api
    #:prototype
    #:define-simple-type
    #:checktype
    ;;;; type
    #:maybe
    #:generalized-boolean
    ;;;; useful helper
    #:nested-cons-type-specifier
    ;;;; condition
    #:invalid-condition-type
    ))
(in-package :type-ext)

(defmacro prototype (name (&rest param-type*) return-type)
  (check-type name symbol)
  (flet((ensure-return-type(return-type)
	  ;; in order to avoid annoying compiler note in SBCL.
	  ;; See 4.2.4 Implementation Limitations in SBCL manual.
	  (if(typep return-type '(CONS(EQL VALUES)T))
	    return-type
	    `(VALUES ,return-type &OPTIONAL))))
    `(DECLAIM(FTYPE (FUNCTION ,param-type* ,(ensure-return-type return-type))
		    ,name))))

(defmacro define-simple-type((name &rest option*) &body body)
  (check-type name (AND SYMBOL (NOT(OR BOOLEAN KEYWORD))))
  (multiple-value-bind(predicate element-type element-predicate)(parse-options option* name)
    (when(and (null element-type) element-predicate)
      (error "Invalid syntax: Specifying :ELEMENT-PREDICATE without specifying :ELEMENT-TYPE. ~S" name))
    (when(and (null element-type)(null body))
      (error "invalid syntax: Missing body. ~S"name))
    (when(and element-type body)
      (error "Invalid syntax: Specifying :ELEMENT-TYPE with specifiying BODY. ~S" name))
    `(EVAL-WHEN(:LOAD-TOPLEVEL :COMPILE-TOPLEVEL :EXECUTE)
       (DECLAIM(INLINE ,predicate))
       ,@(if(null element-type)
	   (the-deftype-form name body predicate)
	   (the-listed-type-form predicate element-predicate name element-type))
       '(,name ,@(may-name predicate) ,@(may-name element-predicate)))))

(defun may-name(function-designator)
  (unless(or (typep function-designator '(CONS (EQL LAMBDA) T))
	     (null function-designator))
    `(,function-designator)))

(defun the-deftype-form(name body predicate)
  `((DEFTYPE,name()
      ,@body)
    (DEFUN,predicate(#0=#:ARG)
      (TYPEP #0# ',name))))

(defun the-listed-type-form(predicate element-predicate name element-type)
  `((DEFUN,predicate(#0=#:ARG)
      (VALUES(IGNORE-ERRORS(EVERY #',element-predicate #0#))))
    (DEFTYPE,name(&OPTIONAL #0#)
      (IF(EQ '* #0#)
	'(SATISFIES ,predicate)
	(NESTED-CONS-TYPE-SPECIFIER ',element-type #0#)))))

(defun parse-options(options name)
  (labels((PREDICATE-NAME(name)
	    (intern(concatenate 'string (symbol-name name) "-P")))
	  (GET-OPTION(type)
	    (second(assoc type options)))
	  (ENSURE(type thunk)
	    (or (GET-OPTION type)
		(funcall thunk)))
	  (INTERNER(element-type)
	    (lambda()
	      (when element-type
		(multiple-value-bind(symbol status)(PREDICATE-NAME element-type)
		  (unless status
		    (warn "~S make new symbol ~S~&May :ELEMENT-PREDICATE option is lacked?" 'define-simple-type symbol))
		  symbol))))
	  )
    (let*((predicate(ENSURE :predicate(lambda()(PREDICATE-NAME name))))
	  (element-type(GET-OPTION :element-type))
	  (element-predicate(ENSURE :element-predicate(INTERNER element-type))))
      (values predicate element-type element-predicate))))

(defun nested-cons-type-specifier(type num)
  (if(<= num 0)
    'null
    `(cons ,type ,(nested-cons-type-specifier type (1- num)))))

(deftype maybe(&optional type)
  (if(eq '* type)
    T
    `(OR NULL ,type)))

(deftype generalized-boolean()
  T)

(define-condition invalid-condition-type(program-error simple-type-error)())

(defmacro checktype(form type condition format-control &rest format-argument*)
  (let((datum(gensym "DATUM")))
    (assert (subtypep condition 'simple-type-error)()
	    'invalid-condition-type
	    :format-control "~S is not subtype of simple-type-error."
	    :format-arguments `(,condition)
	    :expected-type 'simple-type-error
	    :datum condition)
    `(LET((,datum ,form))
       (ASSERT (TYPEP ,datum ',type)()
	       ',condition
	       :FORMAT-CONTROL ,format-control
	       :FORMAT-ARGUMENTS (LIST ,@format-argument*)
	       :EXPECTED-TYPE ',type
	       :DATUM ,datum))))
