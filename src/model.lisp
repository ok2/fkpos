(in-package :fkpos)

(defmodel category categories categories
  ((desc string "")))

(defmodel component components components
  ((descr string "")
   (units keyword :d) ; :g gram, :l liter, :d discrete
   (price (cons real keyword) (cons 0 :NOK))
   (sell? boolean nil))
  ((category t)))

(defmodel usage database component
  ((amount (integer 0 *) 0))
  ((component nil)))

(defmodel customer customers customers
  ((name string "")
   (identification string "")
   (address string "")))

(defmodel transaction transactions transactions
  ((ts ts (timestamp) str)
   (sell? boolean t))
  ((component nil sell?)))

(defmodel payment database transaction
  ((ts ts (timestamp) str)
   (amount (cons real keyword) (cons 0 :NOK))
   (paytype keyword :card))
  ((customer nil)))
