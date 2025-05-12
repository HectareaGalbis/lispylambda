
(defsystem "lispylambda"
  :author "Héctor Galbis Sanchis"
  :license "MIT"
  :description "Tu blog de Lisp en español."
  :defsystem-depends-on ("adp-github")
  :depends-on ("adp" "split-sequence" "trivial-macroexpand-all")
  :class :adp-github
  :serial t
  :components ((:file "package")
               (:scribble "README")
               (:module "src"
                :components ((:file "util")))
               (:module "posts"
                :components ((:module "captura-variable"
                              :components ((:file "commands")
                                           (:scribble "content")))))))
