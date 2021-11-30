(in-package :cl-user)

(defpackage fkpos
  (:nicknames :fkp)
  (:use :cl :let-over-lambda)
  (:export :*curr-db* :*db*
           :new :ls :fv :rm :pp :co :ln :info :obj
           :str :children :ref :timestamp))

(defpackage fkpos-cli
  (:nicknames :fkpc)
  (:use :cl)
  (:export :ls :new :fv :rm :co :pp :info :obj))
