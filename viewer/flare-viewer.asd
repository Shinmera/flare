(asdf:defsystem flare-viewer
  :version "0.1.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Easy particle systems with fine grained control."
  :homepage "https://Shinmera.github.io/flare/"
  :bug-tracker "https://github.com/Shinmera/flare/issues"
  :source-control (:git "https://github.com/Shinmera/flare.git")
  :serial T
  :components ((:file "package")
               (:file "main")
               (:file "menu")
               (:file "viewer")
               (:file "examples"))
  :depends-on (:flare
               :verbose
               :qtools
               :qtcore
               :qtgui
               :qtopengl
               :cl-opengl))
