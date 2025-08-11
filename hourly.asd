(asdf:defsystem #:hourly
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "0.0.0"
  :license "zlib"
  :description "An hour tracking service."
  :homepage "https://shirakumo.org/docs/courier/"
  :bug-tracker "https://shirakumo.org/project/courier/issues"
  :source-control (:git "https://shirakumo.org/project/courier.git")
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "objects")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :relational-database)
               (:interface :auth)
               :r-data-model
               :r-clip
               :i-json
               :alexandria
               :cl-csv))
