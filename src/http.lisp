;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: FKPOS; Base: 10 -*-

(in-package :fkpos)

(defvar *http-server* nil)

(defun parse-path ()
  (let* ((path (split-sequence #\/ (hunchentoot:script-name hunchentoot:*request*)))
         (path (loop for p in path
                     unless (equal p "")
                       collect (find-symbol p :keyword))))
    path))

(defun parse-args (args)
  (loop for a in args
        collect (cond ((and (stringp a) (> (length a) 0) (eql (char a 0) #\:))
                       (or (find-symbol (subseq a 1) :keyword) a))
                      ((consp a) (parse-args a))
                      (t a))))

(defun dump-result (result)
  (cond ((consp result) (cons (dump-result (car result))
                              (dump-result (cdr result))))
        ((keywordp result) (format nil ":~A" (symbol-name result)))
        ((numberp result) result)
        ((eql result t) t)
        ((not result) nil)
        (t (format nil "~A" result))))

(defun pick-command (command)
  (ignore-errors
   (coerce (find-symbol command :fkpos-cli) 'function)))

(hunchentoot:define-easy-handler (fkpos :uri (lambda (r) t))
    (command args)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((state-header "X-KPOS-State")
        (path-header "X-KPOS-Path")
        (cmd-header "X-KPOS-Command")
        (path (parse-path))
        (cmd (and command (find-symbol command :fkpos-cli)))
        (cl-who:*attribute-quote-char* #\"))
    (if (not cmd)
        (prog1 (default-page)
          (setf (hunchentoot:content-type*) "text/html")
          (setf (hunchentoot:header-out state-header)
                (format nil "Command ~A not found" command)))
        (let ((args (when args (parse-args (with-input-from-string
                                         (*json-input* args)
                                             (decode-json)))))
              (*curr-db* *db*))
          (if (apply #'fkpos-cli:co path)
            (with-output-to-string (*json-output*)
              (let* ((path (fkpos-cli:pp))
                     (cmdline (cons cmd args))
                     (error nil)
                     (result (multiple-value-bind (ret err)
                                 (ignore-errors (apply cmd args))
                               (prog1 ret (unless ret (setf error err))))))
                (setf (hunchentoot:header-out state-header)
                      (format nil "~A" error))
                (setf (hunchentoot:header-out path-header)
                      (format nil "~A" path))
                (setf (hunchentoot:header-out cmd-header)
                      (format nil "~A" cmdline))
                (setf (hunchentoot:content-type*) "application/json")
                (encode-json (dump-result result))))
            (prog1 (default-page)
              (setf (hunchentoot:content-type*) "text/html")
              (setf (hunchentoot:header-out state-header)
                    (format nil "Path ~A not found" path))))))))

(push (hunchentoot:create-folder-dispatcher-and-handler "/js/" "./js/" "application/javascript")
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-folder-dispatcher-and-handler "/css/" "./css/" "text/css")
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (fkpos-js :uri "/fkpos/fkpos.js") ()
  (setf (hunchentoot:content-type*) "application/json")
  (default-js))

(hunchentoot:define-easy-handler (fkpos-css :uri "/fkpos/fkpos.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (default-css))

(hunchentoot:define-easy-handler (fkpos-index :uri "/index.html") ()
  (setf (hunchentoot:content-type*) "text/html")
  (default-page))

(hunchentoot:define-easy-handler (fkpos-html :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (default-page))

(defun start (&optional (port 4242))
  (setf *http-server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *http-server*))

(defun stop ()
  (hunchentoot:stop *http-server*))
