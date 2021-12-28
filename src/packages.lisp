;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FKPOS; Base: 10 -*-

(in-package :cl-user)

(defpackage fkpos
  (:nicknames :fkp)
  (:use :cl :let-over-lambda :bordeaux-threads
        :split-sequence :cl-json :parse-number
        :drakma)
  (:import-from :cl-json :*json-input* :decode-json :encode-json)
  (:import-from :parenscript :ps)
  (:import-from :css-lite :css)
  (:import-from :cl-who :with-html-output :with-html-output-to-string :htm)
  (:export :*curr-db* :*db* :*next-id* :init
           :new :ls :fv :rm :pp :co :ln :info :obj
           :repr :children :ref :timestamp :value
           :start :stop))

(defpackage fkpos-cli
  (:nicknames :fkpc)
  (:use :cl)
  (:export :ls :ll :new :fv :rm :co :pp :info :obj :value))
