# TYPE-EXT 0.0.0 - What is this?

* Current lisp world
Common Lisp has strong DEFTYPE features.

* Issues
Sometimes, we need to define type which is list and have same elements.
But ANSI-CL's type specifier '(AND ...) is not specified that be evaluated from left to right and short cut is occur when meet unsatisfied thing.
So we care to such implementation dependent manner.

* Proposal
Define-simple-type provides such solver.

## Usage
```lisp
(define-simple-type(symbols (:element-type symbol)
                            (:element-predicate symbolp)))
=> (SYMBOLS SYMBOLS-P SYMBOLP)

(typep nil 'symbols)
=> T
(typep '(:a :b :c) 'symbols)
=> T
(typep :a 'symbols)
=> NIL
(typep '(:a :b :c) '(symbols 4))
=> NIL
(typep '(:a :b :c) '(symbols 3))
=> T
```

## From developer

* Product's goal
* License
* Developped with - 
* Tested with - 

