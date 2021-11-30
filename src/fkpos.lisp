(in-package :fkpos)

(defun kw (sym) (intern (symbol-name sym) "KEYWORD"))

(defstruct ts
  (year 1970 :type (integer 1 3000))
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (zone 0 :type (rational -24 24)))

(defun timestamp ()
  (multiple-value-bind (second minute hour date month year day dl-p zone)
      (get-decoded-time)
    (make-ts :year year
             :month month
             :day date
             :hour hour
             :minute minute
             :second second
             :zone zone)))

(defgeneric str (o))

(defmethod str ((o ts))
  (let ((z (ts-zone o)))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~C~2,'0D"
            (ts-year o) (ts-month o) (ts-day o) (ts-hour o) (ts-minute o) (ts-second o)
            (if (< z 0) #\- #\+) (abs z))))

(defgeneric ls (o))
(defgeneric new (o ot &optional title))
(defgeneric fv (o field value))
(defgeneric rm (o title))
(defgeneric co (o &optional title))
(defgeneric pp (o))
(defgeneric ln (o n))
(defgeneric info (o))
(defgeneric obj (o path))
(defgeneric red (o))

(defvar *next-id* 1)

(defclass db-base () ())

(defclass database (db-base)
  ((title :initarg :title :accessor title :type keyword)
   (id :initform (incf *next-id*) :reader id :type number)
   (children :initform (make-hash-table :test 'eq) :accessor children :type hash-table)
   (parent :initarg :parent :initform nil :accessor parent)))

(defparameter *objects* (make-hash-table))

(defmethod initialize-instance :after ((o database) &rest initargs)
  (setf (gethash (id o) *objects*) o))

(defmethod info ((o database))
  (with-slots (id title children parent) o
    (list id title
          (class-name (class-of o))
          (hash-table-count children)
          (pp o))))

(defclass root (database) ())
(defclass transactions (database) ())
(defclass components (database) ())
(defclass categories (database) ())
(defclass customers (database) ())

(defmethod initialize-instance :after ((o root) &rest initargs)
  (loop for (name . type) in (list (cons :transactions 'transactions)
                                   (cons :components 'components)
                                   (cons :customers 'customers)
                                   (cons :categories 'categories))
        do (setf (gethash name (children o))
                 (make-instance type :title name :parent o))))

(defparameter *db* (make-instance 'root :title :root))
(defparameter *curr-db* *db*)

(defmethod obj ((o database) path)
  (if (not path) o
      (let* ((p (car path))
             (n (gethash p (children o))))
        (unless (and p n) (error "No object with title ~A found." p))
        (obj (ref n) (cdr path)))))

(defmethod pp ((o database))
  (labels ((p (p) (if p (cons (title p) (p (parent p))) nil)))
    (reverse (p o))))

(defclass reference (db-base)
  ((ref :initarg :ref :reader ref-id :type number)))

(defmethod ref ((o reference)) (gethash (ref-id o) *objects*))
(defmethod ref ((o database)) o)
(defmethod title ((o reference)) (title (ref o)))
(defmethod id ((o reference)) (id (ref o)))
(defmethod children ((o reference)) (id (ref o)))
(defmethod ls ((o reference)) (ls (ref o)))
(defmethod new ((o reference) (ot symbol) &optional title) (new (ref o) ot title))
(defmethod rm ((o reference) (title symbol)) (rm (ref o) title))
(defmethod co ((o reference) &optional title) (co (ref o) title))
(defmethod pp ((o reference)) (pp (ref o)))

(defmethod ls ((o database))
  (loop for c being the hash-value of (children o)
        collect (cons (title c) (id c)) into res
        finally (return (mapcar #'car (sort res #'< :key #'cdr)))))

(defmethod co ((o database) &optional title)
  (setf *curr-db* (cond ((eql title :parent) (parent o))
                        (title (ref (gethash title (children o) o)))
                        (t *db*))))

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
         (let* ((title (if title title (intern (format nil
                                                          ,(format nil "~A-~~D" name)
                                                          (incf *next-id*))
                                                  "KEYWORD")))
                (object (make-instance ',name :title title :parent o)))
           (prog1 title
             (setf (gethash (title object) (children o)) object))))

       ,@(loop for (r b ff) in refs
               for rr = (intern (format nil "~A-REF" r))
               collect
               `(defmethod ln ((o ,name) (n ,r))
                  (let ((id (id n)) (title (title n)))
                    (prog1 title
                      ,@(cond ((not ff) nil)
                              ((symbolp ff)
                               `((unless (,ff n)
                                   (error (format nil "Filter ~A failed." ',ff)))))
                              ((consp ff)
                               `((unless (flet ((x ,@ff)) (x o))
                                   (error (format nil "Filter ~A failed." ',ff))))))
                      (unless (gethash title (children o))
                        (setf (gethash title (children o))
                              (make-instance ',rr :ref id))
                        ,(when b `(ln n o))))))
               when b collect
                 `(defmethod ln ((o ,r) (n ,name))
                    (let ((id (id n)) (title (title n)))
                      (prog1 title
                        (unless (gethash title (children o))
                          (setf (gethash title (children o))
                                (make-instance ',ref :ref id))
                          ,(when b `(ln n o)))))))

       (defmethod info ((o ,name))
         (let ((info (call-next-method))
               (fields (list ,@(loop for (name type default repr) in args append
                                        `(,(kw name)
                                          ,(if repr `(,repr (,name o))
                                               `(,name o)))))))
           (destructuring-bind (id title type children &rest rest) info
             `(,id ,title ,type ,children ,fields ,@rest))))

       (defmethod fv ((o ,name) (field symbol) value)
         (cond ,@(loop for (name type) in args collect
                       `((eql field ,(kw name))
                         (setf (,name o) (coerce value ',type))))
               (t (error (format nil "Unknown field ~A in object ~A" field ',name))))))))

