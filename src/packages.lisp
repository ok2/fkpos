(in-package :cl-user)

(defpackage fkpos
  (:nicknames :fkp)
  (:use :cl :let-over-lambda :bordeaux-threads
        :split-sequence :cl-json :parse-number)
  (:export :*curr-db* :*db* :*next-id* :init
           :new :ls :fv :rm :pp :co :ln :info :obj
           :str :children :ref :timestamp :value
           :start :stop))

(defpackage fkpos-cli
  (:nicknames :fkpc)
  (:use :cl)
  (:export :ls :ll :new :fv :rm :co :pp :info :obj :value))
