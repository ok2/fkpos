(in-package :fkpos)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (declaim (optimize (safety 3) (debug 3) (speed 0) (space 0) (compilation-speed 0))))

(defun units-check (u)
  (cond ((eql u :g) :g)
        ((eql u :l) :l)
        ((eql u :m) :m)
        (t :d)))

(defun paytype-check (u)
  (cond ((eql u :card) :card)
        ((eql u :bank) :bank)
        ((eql u :vips) :vips)
        ((eql u :paypal) :paypal)
        ((eql u :revolut) :revolut)
        (t :contant)))

(defun price-check (p)
  (let* ((p (list2cons p))
         (n (car p))
         (c (cdr p)))
    (cond ((eql c :nok) (cons n c))
          ((eql c :eur) (cons n c))
          ((eql c :usd) (cons n c))
          ((eql c :btc) (cons n c)))))

(defun ident-check (p)
  (cond ((eql p :card) :card)
        ((eql p :address) :address)
        ((eql p :phone) :phone)
        ((eql p :email) :email)
        ((eql p :organisation) :organisation)
        ((eql p :org-number) :org-number)
        (t :ident)))

(defmodel category categories categories
  ((name string "")
   (desc string "")
   (menu boolean nil)      ; appear on menu
   (pos boolean nil)       ; appear in kasse
   (kitchen boolean nil)   ; appear in kitchen
   (property boolean nil)  ; like hot, extra hot, contains alcohol
   (comment string "")))

(defmodel ingredient ingredients ingredients
  ((name string "")
   (descr string "")
   (comment string "")))

(defmodel component components components
  ((name string "")
   (descr string "")
   (units keyword :d nil units-check) ; :g gram, :l liter, :m metre, :d discrete
   (price (cons real keyword) (cons 0 :NOK) cons2list price-check)
   (picture string "")
   (big-picture string "")
   (sell? boolean nil)
   (buy? boolean nil)
   (comment string ""))
  ((category t)
   (ingredient t)))

(defmodel usage database component
  ((amount number 0)
   (comment string ""))
  ((component nil)))

(defmodel customer customers customers
  ((name string "")
   (comment string "")))

(defmodel ident database customer
  ((ident string "")
   (type symbol :ident nil ident-check)))

(defmodel transaction transactions transactions
  ((ts ts (timestamp) ts2list list2ts)
   (sell? boolean t)
   (revoked? boolean nil)
   (finished? boolean nil)
   (discount (cons real keyword) (cons 0 :NOK) cons2list price-check)
   (expire ts (timestamp (* 14 24 60 60)) ts2list list2ts)
   (comment string ""))
  ((customer nil)))

(defmodel order database transactions
  ((amount number 1)
   (tax number *default-tax*)
   (discount (cons real keyword) (cons 0 :NOK) cons2list price-check))
  ((component nil ((o n) (or (and (sell? (parent o)) (sell? n))
                             (and (not (sell? (parent o))) (buy? n)))))))

(defmodel payment database payments
  ((ts ts (timestamp) ts2list list2ts)
   (ident string "")
   (amount (cons real keyword) (cons 0 :NOK) cons2list price-check)
   (receive? boolean t)
   (paytype keyword :card nil paytype-check)
   (comment string ""))
  ((customer nil)
   (transaction t)))

(defmethod value ((o cons) (currency symbol))
  (unless (eql (cdr o) currency)
    (error "Currency not matching: ~S -> ~S" (cdr o) currency))
  (cons (car o) currency))

(defmethod value ((o payment) (currency symbol))
  (value (amount o) currency))

(defmethod value ((o transaction) (currency symbol))
  (let ((value 0))
    (loop for order being each hash-value of (children o)
          if (eql (class-of (ref order)) (find-class 'order)) do
            (let ((v 0))
              (loop for component being each hash-value of (children order)
                    if (eql (class-of (ref component)) (find-class 'component)) do
                      (progn
                        (incf v (car (value (price (ref component)) currency)))))
              (setf v (* v (amount order)))
              (decf v (car (value (discount (ref order)) currency)))
              (incf value v)
              (incf value (* (/ (tax order) 100) v))))
    (loop for payment being each hash-value of (children o)
          if (eql (class-of (ref payment)) (find-class 'payment)) do
            (decf value (car (value (ref payment) currency))))
    (cons value currency)))
