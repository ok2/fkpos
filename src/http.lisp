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

(hunchentoot:define-easy-handler (fkpos :uri (lambda (r) t))
    (command args)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((path (parse-path))
        (command (find-symbol command :fkpos-cli))
        (args (when args (parse-args (with-input-from-string (*json-input* args)
                                       (decode-json)))))
        (*curr-db* *db*))
    (apply #'fkpos-cli:co path)
    (with-output-to-string (*json-output*)
      (encode-json (dump-result (list :path (fkpos-cli:pp)
                                      :command (cons command args)
                                      :result (apply command args)))))))

(defun start (&optional (port 4242))
  (setf *http-server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *http-server*))

(defun stop ()
  (hunchentoot:stop *http-server*))
