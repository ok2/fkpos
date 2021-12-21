(in-package :fkpos-tests)

(def-suite fkpos-tests
  :description "The master test suite of FKPOS.")

(in-suite fkpos-tests)

(defun all-tests () (run! 'fkpos-tests))

(test basic-test
  (let ((prev-db nil))
    (let* ((*next-id* 1)
           (*last-transaction* nil)
           (*db-store* #P"test/test.dat")
           (*objects* (make-hash-table))
           (*curr-db* nil)
           (*db* nil))
      (delete-file *db-store*)
      (init)
      (is (eql (title *db*) :root))
      (co)
      (is (eql (title *curr-db*) :root))
      (co :categories)
      (is (eql (title *curr-db*) :categories))
      (new :category :test1)
      (new :category :test2)
      (co :test2)
      (is (eql (title *curr-db*) :test2))
      (new :category :test3)
      (new :category :test4)
      (is (equal (ls) '(:test3 :test4)))
      (co :parent)
      (is (eql (title *curr-db*) :categories))
      (new :category :test5)
      (co :test5)
      (is (equal (pp) '(:categories :test5)))
      (co :parent)
      (is (equal (ls) '(:test1 :test2 :test5)))
      (co)
      (co :components)
      (new :component :tc1)
      (co :tc1)
      (fv :name "tc1")
      (fv :descr "tc1 descr")
      (fv :price '(10.30 :NOK))
      (fv :sell? t)
      (is (equal (info) '(1000006 :TC1 FKPOS::COMPONENT 0
                          (:NAME "tc1" :DESCR "tc1 descr" :UNITS :D :PRICE (10.3 :NOK)
                           :PICTURE "" :BIG-PICTURE "" :SELL? T :BUY? NIL :COMMENT "")
                          (:COMPONENTS :TC1) T)))
      (ln (obj :categories :test1))
      (ln (obj :categories :test2))
      (is (equal (ll)
                 '((1000001 :TEST1 FKPOS::CATEGORY 1
                    (:NAME "" :DESC "" :MENU NIL :POS NIL :KITCHEN NIL :PROPERTY NIL :COMMENT "")
                    (:CATEGORIES :TEST1) T)
                   (1000002 :TEST2 FKPOS::CATEGORY 3
                    (:NAME "" :DESC "" :MENU NIL :POS NIL :KITCHEN NIL :PROPERTY NIL :COMMENT "")
                    (:CATEGORIES :TEST2) T))))
      (co :parent)
      (new :component :tc2)
      (co :tc2)
      (fv :name "tc2")
      (fv :descr "tc2 descr")
      (fv :price '(20 :NOK))
      (new :usage :tc1usage)
      (co :tc1usage)
      (is (pp) '(:components :tc2 :tc1usage))
      (fv :amount 7)
      (ln (obj :components :tc1))
      (co :parent)
      (ln (obj :categories :test2))
      (ln (obj :categories :test5))
      (co :root :categories)
      (is (pp) '(:categories))
      (rm :test5)
      (co :root :components :tc2)
      (is (equal (ll)
                 '((1000008 :TC1USAGE FKPOS::USAGE 1 (:AMOUNT 7 :COMMENT "")
                    (:COMPONENTS :TC2 :TC1USAGE) T)
                   (1000002 :TEST2 FKPOS::CATEGORY 4
                    (:NAME "" :DESC "" :MENU NIL :POS NIL :KITCHEN NIL :PROPERTY NIL :COMMENT "")
                    (:CATEGORIES :TEST2) T)
                   (1000005 :TEST5 FKPOS::CATEGORY 1
                    (:NAME "" :DESC "" :MENU NIL :POS NIL :KITCHEN NIL :PROPERTY NIL :COMMENT "")
                    (:CATEGORIES :TEST5) NIL))))
      (is (equal (info) '(1000007 :TC2 FKPOS::COMPONENT 3
                          (:NAME "tc2" :DESCR "tc2 descr" :UNITS :D
                           :PRICE (20 :NOK)
                           :PICTURE "" :BIG-PICTURE ""
                           :SELL? NIL :BUY? NIL
                           :COMMENT "")
                          (:COMPONENTS :TC2) T)))
      (is (not (obj :categories :test5)))
      (co :root :transactions)
      (new :transaction :t1)
      (co :t1)
      (new :order :o1)
      (co :o1)
      (fv :amount 4)
      (ln (obj :components :tc1))
      (co :root :customers)
      (new :customer :c1)
      (co :c1)
      (fv :name "Customer name")
      (new :ident :i1)
      (co :i1)
      (fv :category :card)
      (fv :data "1234")
      (co :root :payments)
      (new :payment :p1)
      (co :p1)
      (fv :ident "pppp")
      (fv :amount '(30 :NOK))
      (fv :paytype :contant)
      (co :root :transactions :t1)
      (ln (obj :payments :p1))
      (setf prev-db (loop for val being each hash-key of *objects*
                          collect (cons val (fkpos:info (gethash val *objects*))))))
    (let* ((*next-id* 1)
           (*last-transaction* nil)
           (*db-store* #P"test/test.dat")
           (*objects* (make-hash-table))
           (*curr-db* nil)
           (*db* nil))
      (init)
      (is (equal
           prev-db
           (loop for val being each hash-key of *objects*
                 collect (cons val (fkpos:info (gethash val *objects*)))))))))
