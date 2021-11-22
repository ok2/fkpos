(in-package :cl-user)

(defpackage fkpos
  (:nicknames :fkp)
  (:use :cl))

(in-package :fkpos)

(defgeneric ls (o))
(defgeneric new (o type))
(defgeneric rm (o title))
(defgeneric cd (o &optional title))
(defgeneric pwd (o))
(defgeneric ln (o n))
(defgeneric info (o))

(defvar *next-id* 1)

(defclass db-base () ())

(defclass database (db-base)
  ((title :initarg :name :accessor title)
   (id :initform (incf *next-id*) :reader id)
   (children :initform (make-hash-table :test 'eq) :accessor children)
   (parent :initarg :parent :initform nil :accessor parent)))

(defmethod initialize-instance :after ((o database) &rest initargs)
  (setf (gethash (id o) *objects*) o))

(defmethod info ((o database))
  (with-slots (id title children parent) o
    (list id title (hash-table-count children)
          (and parent (title parent)))))

(defclass root (database) ())
(defclass transactions (database) ())
(defclass components (database) ())
(defclass groups (database) ())

(defmethod initialize-instance :after ((o root) &rest initargs)
  (loop for (name . type) in (list (cons :transactions 'transactions)
                                   (cons :components 'components)
                                   (cons :groups 'groups))
        do (setf (gethash name (children o))
                 (make-instance type :name name :parent o))))

(defparameter *objects* (make-hash-table))
(defparameter *db* (make-instance 'root :name :root))
(defparameter *curr-db* *db*)

(defclass reference (db-base)
  ((ref :initarg :ref :reader ref)))

(defmethod title ((o reference)) (title (ref o)))
(defmethod id ((o reference)) (id (ref o)))
(defmethod children ((o reference)) (id (ref o)))

(defmethod ls ((o reference)) (ls (ref o)))
(defmethod new ((o reference) (type symbol)) (new (ref o) type))
(defmethod rm ((o reference) (title symbol)) (rm (ref o) title))
(defmethod cd ((o reference) &optional title) (cd (ref o) title))
(defmethod pwd ((o reference)) (pwd (ref o)))
(defmethod ln ((o reference) (n database)) (ln (ref o) n))

(defmethod ls ((o database))
  (loop for c being the hash-value of (children o)
        collect (cons (title c) (id c)) into res
        finally (return (mapcar #'car (sort res #'< :key #'cdr)))))

(defmethod pwd ((o database))
  (title *curr-db*))

(defmethod cd ((o database) &optional title)
  (if title (setf *curr-db* (gethash title (children o) o))
    (setf *curr-db* (parent o))))

(defclass group (database)
  ((title :initarg :title :accessor title)))

(defclass group-ref (reference) ())

(defmethod new ((o groups) (title symbol))
  (let ((group (make-instance 'group
                              :title title
                              :parent o)))
    (setf (gethash (title group) (children o)) group)))

(defclass component (database)
  ((title :initarg :title :accessor title)
   (descr :initarg :descr :accessor descr)
   (units :initarg :units :accessor units)
   (price :initarg :price :accessor price)
   (sell :initarg :sell :initform nil :accessor sell)))
(defclass component-ref (reference) ())

(defmethod ln ((o group) (n component))
  (let ((id (id n)))
    (or (gethash id (children o))
        (prog1 (setf (gethash id (children o))
                     (make-instance 'component-ref :ref id))
          (ln n o)))))

(defmethod ln ((o component) (n group))
  (let ((id (id n)))
    (or (gethash id (children o))
        (prog1 (setf (gethash id (children o))
                     (make-instance 'group-ref :ref id))
          (ln n o)))))

(defclass transaction (database)
  ((ts :initarg :ts :accessor ts)
   (sell :initarg :sell :initform nil :accessor sell)))
(defclass transaction-ref (reference) ())

(defmethod ln ((o transaction) (n component))
  (let ((id (id n)))
    (setf (gethash id (children o))
          (make-instance 'component-ref :id id))))

(defpackage fkpos-cli
  (:nicknames :fkpc)
  (:use :cl)
  (:export :ls :new :edit :rm :cd :pwd))

(in-package :fkpos-cli)

(defun ls () (fkpos::ls fkpos::*curr-db*))
(defun new (title) (fkpos::new fkpos::*curr-db* title))
(defun edit (name) (fkpos::ls fkpos::*curr-db* name))
(defun rm (name) (fkpos::ls fkpos::*curr-db* name))
(defun cd (&optional name) (fkpos::cd fkpos::*curr-db* name))
(defun pwd () (fkpos::pwd fkpos::*curr-db*))
(defun ln (n) (fkpos::ln fkpos::*curr-db* n))
(defun info () (fkpos::info fkpos::*curr-db*))
