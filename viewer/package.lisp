(in-package #:cl-user)
(defpackage #:flare-viewer
  (:nicknames #:org.shirakumo.flare.viewer)
  (:use #:cl+qt #:3d-vectors #:flare)
  (:shadowing-import-from #:flare #:slot)
  (:export #:main))
