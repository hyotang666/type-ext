; vim: ft=lisp et
(in-package :asdf)

(defsystem :type-ext
  :description "Tiny extensions about type."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :components((:file "type")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "type-ext"))))
  (append (call-next-method)'((test-op "type-ext.test"))))
