#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem flare-viewer
  :version "0.1.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Easy particle systems with fine grained control."
  :homepage "https://github.com/Shinmera/flare"
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
