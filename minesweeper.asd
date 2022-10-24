(require "asdf")

(asdf:defsystem :minesweeper
  :description "a simple minesweeper"
  :version "1.0"
  :author "Jakub"
  :depends-on (:ltk)
  :components ((:file "minesfield" :depends-on ("package"))
	       (:file "package")))
