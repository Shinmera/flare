(asdf:defsystem flare
  :version "1.1.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Easy particle systems with fine grained control."
  :homepage "https://shinmera.com/docs/flare/"
  :bug-tracker "https://shinmera.com/project/flare/issues"
  :source-control (:git "https://shinmera.com/project/flare.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "queue")
               (:file "indexed-set")
               (:file "easings")
               (:file "clock")
               (:file "container")
               (:file "paintable")
               (:file "animation")
               (:file "change")
               (:file "parser")
               (:file "scene")
               (:file "forms")
               (:file "documentation"))
  :depends-on (:lambda-fiddle
               :array-utils
               :trivial-garbage
               :3d-vectors
               :documentation-utils
               :for))
