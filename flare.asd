(asdf:defsystem flare
  :version "1.1.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Easy particle systems with fine grained control."
  :homepage "https://Shinmera.github.io/flare/"
  :bug-tracker "https://github.com/Shinmera/flare/issues"
  :source-control (:git "https://github.com/Shinmera/flare.git")
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
