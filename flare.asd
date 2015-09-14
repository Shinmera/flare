#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem flare
  :version "0.1.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Easy particle systems with fine grained control."
  :homepage "https://github.com/Shinmera/flare"
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
               (:file "forms"))
  :depends-on (:lambda-fiddle
               :iterate
               :array-utils
               :trivial-garbage
               :3d-vectors))
