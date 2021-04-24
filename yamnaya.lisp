;;;; yamnaya.lisp

(in-package #:yamnaya)

(defvar *current-stream*)
(defvar *context*)

(defclass context ()
  ((indent :initarg :indent)
  (parent :initarg :parent)))

(defclass yaml-list (context) ())
(defclass yaml-object (context) ())

(defmacro with-yaml-stream (stream &body body)
  `(f-with-yaml-stream ,stream (lambda () ,@body)))

(defun f-with-yaml-stream (stream fn)
  (let ((*current-stream* stream)
	(*context* (make-context)))
    (funcall fn)
    (fresh-line *current-stream*)))

(defun message-separator ()
  (yaml-format "---"))

(defun message-terminator ()
  (yaml-format "..."))

(defun make-context ()
  (make-instance 'context :indent 0 :parent nil))

(defun indent (&optional (context *context*))
  (make-instance 'context :indent (1+ (slot-value context 'indent)) :parent context))

(defun yaml-format (format-directive &rest format-args)
  (fresh-line *current-stream*)
  (dotimes (n (* 2 (slot-value *context* 'indent))) (write-char #\space *current-stream*))
  (apply #'format *current-stream* format-directive format-args))

(defun encode-object-member (key value)
  (yaml-format "~A: ~A" (encode-scalar key) (encode-scalar value)))

(defun encode-scalar (str)
  (concatenate 'string "'" str "'"))

(defun encode-list-member (scalar)
  (yaml-format "- ~A" (encode-scalar scalar)))

(defmacro with-list-member (&body body)
  `(f-with-list-member (lambda () ,@body)))

(defun f-with-list-member (fn)
  (yaml-format "- ")
  (let ((*context* (indent)))
    (funcall fn)))

(defmacro with-object-member (key &body body)
  `(f-with-object-member ,key (lambda () ,@body)))

(defun f-with-object-member (key fn)
  (yaml-format "~A:" (encode-scalar key))
  (let ((*context* (indent)))
    (funcall fn)))

(defun encode-comment (comment)
  (yaml-format "# ~A" comment))

(defun append-comment (comment)
  (format *current-stream* " # ~A" comment))

(defun foo ()
  (with-output-to-string (s)
    (with-yaml-stream s
      (encode-comment "Here's some header material")
      (message-separator)
      (encode-list-member "foo")
      (encode-list-member "bar")
      (append-comment "Comment on bar")
      (with-list-member
	(append-comment "List comment")
	(encode-list-member "indented")
	(encode-list-member "baz"))
      (with-list-member
	(encode-object-member "key" "val")
	(append-commenT "Comment after first key")
	(encode-comment "A comment on key2")
	(encode-object-member "key2" "val2")
	(with-object-member "complex"
	  (encode-list-member "nested")
	  (encode-list-member "list")))
      (message-terminator))))
