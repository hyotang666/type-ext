; vim: ft=lisp et
(in-package :asdf)
(defsystem :type-ext.test :depends-on (:jingoh "type-ext") :components
 ((:file "type-ext")) :perform
 (test-op (o c) (symbol-call :jingoh :examine)))