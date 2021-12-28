(defsystem :fkpos
  :description "Point of sales system for cafe Fjærkroa"
  :version "0.0.1"
  :author "Oleksandr Kozachuk"
  :license "WTFPL"
  :depends-on ("let-over-lambda"
               "bordeaux-threads"
               "hunchentoot"
               "fiveam"
               "split-sequence"
               "cl-json"
               "parse-number"
               "cl-who"
               "drakma"
               "parenscript"
               "css-lite")
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "fkpos" :depends-on ("packages"))
                             (:file "model" :depends-on ("fkpos"))
                             (:file "cli" :depends-on ("model" "fkpos"))
                             (:file "page" :depends-on ("fkpos"))
                             (:file "http" :depends-on ("page"))))
               (:module "test"
                :components ((:file "package")
                             (:file "main" :depends-on ("package")))))
  :perform (test-op (o s) (symbol-call :fkpos-tests :all-tests)))
