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
  (upload-vm1 5)
  ;; i2c
  (ddc-top 0)
  (ddc-bottom 1)
  (i2c-top 2)
  (i2c-bottom 3)
  (vm0-program-memory #x3000))



(define-alien-type u8 unsigned-char)
(define-alien-type u32 unsigned-int)
(define-alien-type card (* int))
(define-alien-type upload int)
(define-alien-type i2c int)
(define-alien-type head int)
(define-alien-type nil
    (struct mode-info
	    (pixel-clock u32)
	    (hres u32) (hfp u32) (hsync u32) (hblank u32)
	    (vres u32) (vfp u32) (vsync u32) (vblank u32)
	    (digital u8)
	    (hsync-is-low u8)
	    (vsync-is-low u8)
	    (pix-depth u8)
	    (fb-start u32)
	    (pitch u32)))
(define-alien-type mode-info-ptr (* (struct mode-info)))
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

(defc set-mode void ((head head) (inf mode-info-ptr) (program (* u32))))

(defc set-video-clock void ((head head) (digital int) (pixel-clock u32)))

(defc load-video-program void ((head head) (inf mode-info-ptr) 
			       (program (* u32))))

(defc set-video-mode void ((head head) (digital int) (pixel-clock u32)
			   (pixel-depth u32)))

(defc dvi-init void ((head head) (pixel-clock u32)))

(defc dvi-init-single-link void ((head head) (pixel-clock u32)))

(defc dvi-init-dual-link void ((head head) (pixel-clock u32)))

(defc edid-find-video-mode int
  ((info mode-info-ptr)
   (width u32) (height u32) (refresh u32)
   (ddc (* u8))))

(defc i2c-init void ((i2c i2c)))
(defc i2c-write2 int ((i2c i2c) (addr u8) (b0 u8) (b1 u8)))
(defc i2c-get-edid int ((head head) (ddc (* u8))))

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

#+nil
(defparameter *card* (card-open #x0b 1 0))
#+nil
(card-mem-init *card*)
#+nil
(let ((ddc (make-array 128 :element-type '(unsigned-byte 8))))
 (defparameter *top* (i2c-get-edid *card* +head-top+ (sb-sys:vector-sap ddc)))
 ;; 0 success, 1 timeout, 2 bad head, 3 bad header, 4 bad checksum
 (defparameter *ddc* ddc))

(defun parse-ddc ()
 (macrolet ((q (i)
	      `(aref *ddc* ,i))
	    (active (i)
	      `(+ (* 256 (ldb (byte 4 4) (q ,(+ 2 i)))) (q ,i)))
	    (blank (i)
	      `(+ (* 256 (ldb (byte 4 0) (q ,(1+ i)))) (q ,i))))
   (list :pixel-clock-hz (* 10000 (+ (* 256 (q 55))
				     (q 54)))
	 :hact (active 56)
	 :hblank (blank 57)
	 :vact (active 59)
	 :vblank (blank 60)
	 :hsync-off (+ (* 256 (ldb (byte 2 6) (q 65))) (q 62))
	 :hsync-width (+ (* 256 (ldb (byte 2 4) (q 65))) (q 63))
	 :vsync-off (+ (* 16 (ldb (byte 2 2) (q 65))) (ldb (byte 4 4) (q 64)))
	 :vsync-width (+ (* 16 (ldb (byte 2 0) (q 65))) (ldb (byte 4 0) (q 64)))
	 :xsize-mm (+ (* 256 (ldb (byte 4 4) (q 68))) (q 66))
	 :ysize-mm (+ (* 256 (ldb (byte 4 0) (q 68))) (q 67))
	 :hborder (q 69)
	 :vborder (q 70)
	 :interlaced (ldb (byte 1 7) (q 71))
	 :stereo (ldb (byte 2 5) (q 71))
	 :separate-sync (ldb (byte 2 3) (q 71))
	 :vsync-positive (ldb (byte 1 2) (q 71))
	 :hsync-positive (ldb (byte 1 1) (q 71))
	 :stereo (ldb (byte 1 0) (q 71)))))

#+nil
(parse-ddc)
#+nil
(defparameter *inf* (make-alien (struct mode-info)))

#+nil
(defun run ()
 (sb-sys:with-pinned-objects (*ddc*)
   (with-alien ((inf (struct mode-info)))
     (defparameter *find-vid* (edid-find-video-mode 
			       *card* (addr inf)
			       0 0 0 (sb-sys:vector-sap *ddc*)))
     (slot inf 'hsync-is-low))))
#+nil



(run)
#+nil ;; turn DAC off
(video-reset *card* 1)
#+nil
(set-video-clock *card* +head-top+ 1 108090000)
;;                                   165000000
#+nil
(dvi-init *card* +head-top+ 108090000) ;; oga1/oga1-utils.c
#+nil
(load-video-program )
#+nil
(let ((program (make-array 512 :element-type '(unsigned-byte 32))))
  (set-mode *card* +head-top+ (sb-sys:vector-sap program)))

(define-alien-routine progressive int
  (program (* u32))
  (fb-width u32)
  (fb-height u32)
  (vp-width u32)
  (vp-height u32)
  (depth u32)
  (hfp u32)
  (hsync u32)
  (hbp u32)
  (vfp u32)
  (vsync u32)
  (vbp u32)
  (viewport u32)
  (pixperclock u32)
  (horiz-assert-low u32)
  (vert-assert-low u32))

#+nil
(defc vm-upload void ((head head) (base u32) (vbase (* u32)) (size size_t)))

(defun lisp-vm-upload (base program n)
  (declare (type (unsigned-byte 32) base n)
	   (type (simple-array (unsigned-byte 32) (512)) program))
 (let ((addr +vm0-program-memory+))
   (incf addr (* 4 base))
   (dotimes (i n)
     (card-rset *card* addr (aref program i))
     (incf addr 4))))


#+nil
(let* ((program (make-array 512 :element-type '(unsigned-byte 32)))
       (sap (sb-sys:vector-sap program))
       (n   (progressive sap
			 1280 1024
			 1280 1024
			 32 
			 48 112 248
			 1 3 38
			 #x80 2
			 0 0)))
  (lisp-vm-upload 0 program n))

(set-video-mode *card* +head-top+ 1 108090000 32)