(defsystem "fkpos"
  :description "Point of sales system for cafe Fjærkroa"
  :version "0.0.1"
  :author "Oleksandr Kozachuk"
  :license "WTFPL"
  :depends-on ("woo" "let-over-lambda" "hunchentoot" "fiveam")
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "fkpos" :depends-on ("packages"))
                             (:file "model" :depends-on ("fkpos"))
                             (:file "cli" :depends-on ("model" "fkpos"))))
               (:module "test"
                :components ((:file "package")
                             (:file "main" :depends-on ("package")))))
  :perform (test-op (o s) (uiop:symbol-call :fiveam :run! 'fkpos-tests:all-tests)))
