(defpackage :oga1-ffi
  (:use :cl :sb-alien :sb-c-call))
(in-package :oga1-ffi)

(load-shared-object "/opt/ogp/lib/liboga1.so")

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
(define-alien-type upload int)
(define-alien-type head int)
(define-alien-type mode-info-struct 
    (struct mode-info-struct
	    (pixel-clock u32)
	    (hres u32) (hfp u32) (hsync u32) (hblank u32)
	    (vres u32) (vfp u32) (vsync u32) (vblank u32)
	    (digital u8)
	    (hsync-is-low u8)
	    (vsync-is-low u8)
	    (pix-depth u8)
	    (fb-start u32)
	    (pitch u32)))
(define-alien-type mode-info (* mode-info-struct))
(define-alien-type size_t unsigned-long)

(eval-when (:compile-toplevel)
  (defun replace-_ (str)
   (dotimes (i (length str) str)
     (when (eq (char str i) #\-)
       (setf (char str i) #\_)))))

(defmacro defc (name ret args &key (card t))
  (progn
    (when card
      (push '(card card) args))
   `(define-alien-routine (,(string-downcase 
			     (replace-_ (format nil "oga1_~a" name)))
			    ,(intern (format nil "~a" name)))
	,ret
      ,@args)))
 


(defc card-rset void ((reg u32) (value u32)))
(defc card-mset void ((reg u32) (value u32)))
(defc card-rget u32 ((reg u32)))
(defc card-mget u32 ((reg u32)))
(defc card-mcopy void ((addr u32) (data (* u8)) (length u32)))

(defc pci-read-long u32 ((addr u32)))
(defc pci-write-long void ((addr u32) (val u32)))
(defc card-rset-config void ((reg u32) (val u32)))
(defc card-rget-config u32 ((addr u32)))
(defc card-mset-config void ((addr u32) (val u32)))

(defc card-erase void ((where upload)))
(defc card-upload void ((where upload) (base u32) (vbase (* int)) 
			(size size_t)))

(defc card-hq-start void ((bypass int)))
(defc card-hq-stop void ())

(defc card-hq-dmem-set void ((base u32) (data u32)))
(defc card-hq-dmem-get u32 ((base u32)))
(defc card-hq-dmem-set-config void ((base u32) (data u32)))
(defc card-hq-dmem-get-config u32 ((base u32)))

(defc card-mem-init void ())

(defc get-clock-dividers u32 ((freq u32)))
(defc get-clock-frequency u32 ((dividers u32)))

(defc video-reset void ((top-digital int)))

(defc set-mode void ((head head) (inf mode-info) (program (* u32))))

(defc set-video-clock void ((head head) (digital int) (pixel-clock u32)))

(defc load-video-program void ((head head) (inf mode-info) (program (* u32))))

(defc set-video-mode void ((head head) (digital int) (pixel-clock u32)
			   (pixel-depth u32)))

(defc dvi-init void ((head head) (pixel-clock u32)))

(defc dvi-init-single-link void ((head head) (pixel-clock u32)))

(defc dvi-init-dual-link void ((head head) (pixel-clock u32)))

(defc edid-find-video-mode int
  ((info (* mode-info))
   (width u32) (height u32) (refresh u32)
   (ddc (* u8))))

;; check how to setup the screen in oga1-hq-test
;; obtain the numbers with the nvidia card
;; use OG_TEST_COMMAND to fill a rectangle on the screen

;; user

(load-shared-object "/opt/ogp/lib/liboga1-user.so")

(defc card-open card ((bus int) (dev int) (func int)) :card nil)
(defc card-free void nil)
(defc card-set-log-file void ((file-pointer (* int))))
(defc edid-dump void ((file-pointer (* int))
		      (ddc (* u8))))