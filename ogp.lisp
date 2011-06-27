(defpackage :oga1-ffi
  (:use :cl :sb-alien :sb-c-call))
(in-package :oga1-ffi)

(defparameter *library* "/opt/ogp/lib/liboga1.so")
(load-shared-object *library*)

(defmacro defconstants (&rest rest)
  `(progn
     ,@(loop for e in rest collect
	    (destructuring-bind (name val) e
	      `(defconstant ,(intern (format nil "+~A+" name))
		 ,val)))))

(defconstants
    (vmem-reg 0)
    (vmem-mem 1)
  (head-top 0)
  (head-bottom 1)
  (upload-pbrom 0)
  (upload-cprom 1)
  (upload-hqprog 2)
  (upload-hqmem 3)
  (upload-vm0 4)
  (upload-vm1 5))

(define-alien-type u8 unsigned-char)
(define-alien-type u32 unsigned-int)
(define-alien-type card (* int))


(defun replace-_ (str)
  (dotimes (i (length str) str)
    (when (eq (char str i) #\-)
      (setf (char str i) #\_))))

(defmacro defcard (name ret args)
  `(define-alien-routine (,(string-downcase 
			    (replace-_ (format nil "oga1_card_~a" name)))
			  ,(intern (format nil "CARD-~a" name)))
       ,ret
     ,@args)) 


(defcard rset void ((card card) (reg u32) (value u32)))
(defcard mset void ((card card) (reg u32) (value u32)))
(defcard rget u32 ((card card) (reg u32)))
(defcard mget u32 ((card card) (reg u32)))
(defcard mcopy void ((card card) (addr u32) (data (* u8)) (length u32)))
