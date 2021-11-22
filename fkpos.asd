(defsystem "fkpos"
  :version "0.0.1"
  :author "Oleksandr Kozachuk"
  :license "WTFPL"
  :depends-on ("woo")
  ;:depends-on ("woo" "let-over-lambda")
  :components ((:module "src"
                :components ((:file "fkpos"))))
  :description "Point of sales system for cafe Fjærkroa"
  :in-order-to ((test-op (test-op "fkpos-test"))))
