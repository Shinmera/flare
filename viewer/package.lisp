#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:flare-viewer
  (:nicknames #:org.shirakumo.flare.viewer)
  (:use #:cl+qt #:3d-vectors #:flare)
  (:shadowing-import-from #:flare #:slot)
  (:export #:main))
