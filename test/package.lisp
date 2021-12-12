(in-package :cl-user)

(defpackage :fkpos-tests
  (:use :cl :fiveam)
  (:import-from :fkpos
   :*db* :*curr-db* :*next-id* :*last-transaction* :*objects*
   :*db-store* :title :root :init)
  (:import-from :fkpos-cli
   :co :new :ls :ll :ts :fv :rm :pp :ln :info :obj)
  (:export :run! :all-tests))
