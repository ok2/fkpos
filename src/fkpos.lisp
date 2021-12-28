;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FKPOS; Base: 10 -*-

(in-package :fkpos)

(defun kw (sym)
  (intern (symbol-name sym) "KEYWORD"))

(defun cons2list (x)
  (list (car x) (cdr x)))

(defun list2cons (x)
  (cons (first x) (second x)))

(defstruct ts
  (year 1970 :type (integer 1 3000))
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (zone 0 :type (rational -24 24)))

(defun timestamp (&optional (offset 0))
  (multiple-value-bind (second minute hour date month year day dl-p zone)
      (decode-universal-time (+ (get-universal-time) offset))
    (declare (ignore day dl-p))
    (make-ts :year year
             :month month
             :day date
             :hour hour
             :minute minute
             :second second
             :zone zone)))

(defgeneric repr (o))
(defgeneric ut (o))
(defgeneric ts2list (o))

(defmethod repr ((o ts))
  (let ((z (ts-zone o)))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~C~2,'0D"
            (ts-year o) (ts-month o) (ts-day o) (ts-hour o) (ts-minute o) (ts-second o)
            (if (< z 0) #\- #\+) (abs z))))

(defmethod ts2list ((o ts))
  (with-slots (year month day hour minute second zone) o
    (list year month day hour minute second zone)))

(defun list2ts (ts)
  (destructuring-bind (year month day &optional hour minute second zone) ts
    (make-ts :year year
             :month month
             :day day
             :hour (or hour 0)
             :minute (or minute 0)
             :second (or second 0)
             :zone (or zone 0))))

(defmethod ut ((o ts))
  (with-slots (year month day hour minute second zone) o
    (encode-universal-time second minute hour day month year zone)))

; read methods
(defgeneric ls (o))
(defgeneric co (o &optional title))
(defgeneric pp (o))
(defgeneric info (o))
(defgeneric obj (o path))
(defgeneric value (o currency &key))

; write methods
(defgeneric new (o ot &optional title))
(defgeneric rm (o title))
(defgeneric ln (o n))
(defgeneric fv (o field value))

(defgeneric replay-transactions (o &optional start stop))

(defparameter *db-store* #P"db.data")
(defparameter *default-tax* 25)

(defvar *next-id* 1)
(defvar *new-id* nil)
(defvar *transaction-skip* nil)
(defvar *last-transaction* nil)
(defvar *curr-db* nil)
(defvar *db* nil)
(defvar *objects* nil)
(defvar *db-lock* (make-lock))

(defun next-id ()
  (if *new-id*
      (prog1 *new-id*
        (setf *next-id* (+ 1 (max *next-id* *new-id*))))
      (incf *next-id*)))

(defclass db-base () ())

(defclass database (db-base)
  ((title :initarg :title :accessor title :type keyword)
   (id :initform (next-id) :initarg :id :reader id :type number)
   (children :initform (make-hash-table :test 'eq) :accessor children :type hash-table)
   (parent :initarg :parent :initform nil :accessor parent)))

(defmethod initialize-instance :after ((o database) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (id o) *objects*) o))

(defun write-transaction (name o ret &rest args)
  (let ((transaction `(,(ts2list (timestamp -1))
                       ,(id o) ,name ,args ,ret)))
    (prog1 ret
      (unless *transaction-skip*
        (with-lock-held (*db-lock*)
          (setf *last-transaction* (timestamp))
          (with-open-file (fd *db-store* :direction :output
                                         :if-exists :append
                                         :if-does-not-exist :create)
            (format fd "~S~%" transaction)))))))

(defmethod new :around ((o database) ot &optional title)
  (funcall #'write-transaction 'new o (call-next-method) ot title))

(defmethod ln :around ((o database) (n database))
  (funcall #'write-transaction 'ln o (call-next-method) (id n)))

(defmethod fv :around ((o database) (field symbol) value)
  (funcall #'write-transaction 'fv o (call-next-method) field value))

(defmethod rm :around ((o database) (title symbol))
  (funcall #'write-transaction 'rm o (call-next-method) title))

(defmethod ln ((o database) (n number))
  (ln o (gethash n *objects*)))

(defmethod rm ((o database) (title symbol))
  (let* ((objects (children o))
         (child (gethash title objects)))
    (when child
      (remhash title objects)
      (id child))))

(defmethod replay-transactions ((o database) &optional start stop)
  (let ((*transaction-skip* t)
        (executed-transactions 0)
        (start (and start (ut start)))
        (stop (and stop (ut stop))))
    (with-open-file (fd *db-store* :direction :input
                                   :if-does-not-exist nil)
      (when fd
        (loop (let ((data (read fd nil nil))
                    (*transaction-skip* t))
                (unless data (return))
                (destructuring-bind (ts parent op args id) data
                  (let* ((ts (ut (funcall #'list2ts ts))))
                    (when (or (not *last-transaction*)
                              (and (>= ts (ut *last-transaction*))
                                   (or (not start) (>= ts start))
                                   (or (not stop) (<= ts stop))))
                      (let ((*new-id* id)
                            (*curr-db* (gethash parent *objects*)))
                        (apply (symbol-function op) *curr-db* args)
                        (incf executed-transactions)))))))))
    (setf *last-transaction* (timestamp))
    executed-transactions))

(defmethod info ((o database))
  (with-slots (id title children parent) o
    (let* ((pp (pp o))
           (obj (obj *db* pp)))
      (list id title
            (class-name (class-of o))
            (hash-table-count children)
            pp
            (and obj (eql (id obj) id) t)))))

(defclass root (database) ())
(defclass transactions (database) ())
(defclass components (database) ())
(defclass categories (database) ())
(defclass customers (database) ())
(defclass payments (database) ())
(defclass ingredients (database) ())

(defmethod initialize-instance :after ((o root) &rest initargs)
  (declare (ignore initargs))
  (loop for (name type id) in (list (list :transactions 'transactions 1)
                                    (list :components 'components 2)
                                    (list :customers 'customers 3)
                                    (list :categories 'categories 4)
                                    (list :payments 'payments 5)
                                    (list :ingredients 'ingredients 6))
        do (setf (gethash name (children o))
                 (make-instance type :id id :title name :parent o)))
  (replay-transactions o))

(defmethod obj ((o database) path)
  (if (not path) o
      (let* ((p (car path))
             (n (gethash p (children o))))
        (and (and p n) (obj (ref n) (cdr path))))))

(defmethod pp ((o database))
  (labels ((p (p) (unless (or (not p) (eql (title p) :root))
                      (cons (title p) (p (parent p))))))
    (reverse (p o))))

(defclass reference (db-base)
  ((ref :initarg :ref :reader ref-id :type number)))

(defmethod info ((o reference))
  (list (slot-value o 'id) (title o)
        (class-name (class-of o))
        0
        (pp o)))

(defmethod ref ((o reference)) (gethash (ref-id o) *objects*))
(defmethod ref ((o database)) o)
(defmethod title ((o reference)) (title (ref o)))
(defmethod id ((o reference)) (id (ref o)))
(defmethod children ((o reference)) (id (ref o)))
(defmethod ls ((o reference)) (ls (ref o)))
(defmethod co ((o reference) &optional title) (co (ref o) title))
(defmethod pp ((o reference)) (pp (ref o)))
(defmethod new ((o reference) (ot symbol) &optional title) (new (ref o) ot title))
(defmethod rm ((o reference) (title symbol)) (rm (ref o) title))
(defmethod fv ((o reference) (field symbol) value) (fv (ref o) field value))
(defmethod value ((o reference) (currency symbol) &key) (value (ref o) currency))

(defmethod ls ((o database))
  (loop for c being the hash-value of (children o)
          collect (cons (title c) (id c)) into res
          finally (progn
                    (stable-sort res #'< :key #'cdr)
                    (return (mapcar #'car res)))))

(defmethod co ((o database) &optional title)
  (setf *curr-db* (cond ((eql title :parent) (parent o))
                          (title (ref (gethash title (children o) o)))
                          (t *db*))))

(defmacro wrap-with-lock (method &rest args)
  `(defmethod ,method :around ((o db-base) ,@args)
     (declare (ignore o ,@(loop for a in args
                                unless (eql (char (symbol-name a) 0) #\&)
                                  collect a)))
     (with-recursive-lock-held (*db-lock*)
       (call-next-method))))

(wrap-with-lock ref)
(wrap-with-lock title)
(wrap-with-lock id)
(wrap-with-lock children)
(wrap-with-lock ls)
(wrap-with-lock co &optional title)
(wrap-with-lock info)
(wrap-with-lock pp)
(wrap-with-lock obj path)
(wrap-with-lock initialize-instance &rest args)

(defmacro defmodel (name base parent args &optional refs)
  (let ((ref (intern (format nil "~A-REF" name))))
    `(progn
       (defclass ,name (,base)
         ,(loop for (name type default) in args collect
                `(,name :initarg ,(kw name)
                        :type ,type
                        :initform ,default
                        :accessor ,name)))
       (defclass ,ref (reference) ())

       (defmethod new ((o ,parent) (ot (eql ,(kw name))) &optional title)
         (let* ((id (next-id))
                (title (if title title (intern (format nil ,(format nil "~A-~~D" name) id)
                                               "KEYWORD")))
                (object (make-instance ',name :id id :title title :parent o)))
           (prog1 id
             (setf (gethash (title object) (children o)) object))))

       ,@(loop for (r b ff) in refs
               for rr = (intern (format nil "~A-REF" r))
               collect
               `(defmethod ln ((o ,name) (n ,r))
                  (let ((id (id n)) (title (title n)))
                    (prog1 id
                      ,@(cond ((not ff) nil)
                              ((symbolp ff)
                               `((unless (,ff n)
                                   (error (format nil "Filter ~A failed." ',ff)))))
                              ((consp ff)
                               `((unless (flet ((x ,@ff)) (x o n))
                                   (error (format nil "Filter ~A failed." ',ff))))))
                      (unless (gethash title (children o))
                        (setf (gethash title (children o))
                              (make-instance ',rr :ref id))
                        ,(when b
                           `(when (not (gethash (title o) (children n)))
                              (ln n o)))))))
               when b collect
                 `(defmethod ln ((o ,r) (n ,name))
                    (let ((id (id n)) (title (title n)))
                      (prog1 id
                        ,@(cond ((not ff) nil)
                                ((symbolp ff)
                                 `((unless (,ff o)
                                     (error (format nil "Filter ~A failed." ',ff)))))
                                ((consp ff)
                                 `((unless (flet ((x ,@ff)) (x n o))
                                     (error (format nil "Filter ~A failed." ',ff))))))
                        (unless (gethash title (children o))
                          (setf (gethash title (children o))
                                (make-instance ',ref :ref id))
                          (when (not (gethash (title o) (children n)))
                            (ln n o)))))))

       (defmethod info ((o ,name))
         (let ((info (call-next-method))
               (fields (list ,@(loop for (name type default repr) in args append
                                        `(,(kw name)
                                          ,(if repr `(,repr (,name o))
                                               `(,name o)))))))
           (destructuring-bind (id title type children &rest rest) info
             `(,id ,title ,type ,children ,fields ,@rest))))

       (defmethod fv ((o ,name) (field symbol) value)
         (cond ,@(loop for (name type default repr parse) in args collect
                       `((eql field ,(kw name))
                         (setf (,name o)
                               (coerce ,(if (not parse)
                                            'value
                                            (list parse 'value))
                                       ',type))))
               (t (error (format nil "Unknown field ~A in object ~A" field ',name))))
         (id o)))))

(defun init ()
  (setf *last-transaction* nil)
  (setf *objects* (make-hash-table))
  (setf *db* (make-instance 'root :title :root))
  (setf *curr-db* *db*)
  (setf *next-id* 1000000))
