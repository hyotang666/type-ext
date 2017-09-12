; vim: ft=lisp et
(in-package :asdf)

(defsystem :type-ext
  :description "Tiny extensions about type."
  :long-description #.(uiop:read-file-string
                        (uiop:subpathname *load-pathname* "README.md"))
  :author "Shinichi Sato"
  :license "MIT"
  :components((:file "type")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "type-ext"))))
 (test-system :type-ext.test))
