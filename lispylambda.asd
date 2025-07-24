
(defsystem "lispylambda"
  :author "Héctor Galbis Sanchis"
  :license "MIT"
  :description "Tu blog de Lisp en español."
  :defsystem-depends-on ("adp-github")
  :depends-on ("adp" "split-sequence")
  :class :adp-github
  :serial t
  :components ((:file "package")
               (:scribble "README")
               (:module "posts"
                :components ((:scribble "captura-variable/content")
                             (:scribble "macros-with/content")))))
