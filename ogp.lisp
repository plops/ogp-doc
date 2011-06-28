(defpackage :oga1-ffi
  (:use :cl :sb-alien :sb-c-call))
(in-package :oga1-ffi)

(load-shared-object "/opt/ogp/lib/liboga1.so")

(defparameter *ddc* 0)
(defparameter *card* 0)

(defmacro defconstants (&rest rest)
  "Define multiple constants. Reduces the amount that needs to be
written."
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
  ;; constants from reg_defines
  ;; block s3
  (s3-pll-min 180000000)
  (s3-pll-max 360000000)
  (s3-ref-clock0 133330000)
  (s3-ref-clock1 125000000)
  ;; block vm
  (vm0-program-memory #x3000)
  (vm1-program-memory #x4000)
  ;; block vc
  (vc0-pixel-x-start #x3800)
  (vc0-pixel-y-start #x3804)
  (vc0-clear-int #x3808)
  (vc0-pixel-info #x380c) 
  (vc0-cpu-reset-b #x3810)
  (vc0-interrupt-enable #x3814)
  (vc0-video-enable #x3818)
  (vc0-pc-start #x3820)
  (vc0-dividers #x3824)
  (vc0-output-mode #x3828)
  ;; dividers
  (dividers-divisor1-mask 7)
  (dividers-divisor1-lsb-posn 0)
  (dividers-divisor1-msb-posn 2)
  (dividers-divisor0-mask #x1f8)
  (dividers-divisor0-lsb-posn 3)
  (dividers-divisor0-msb-posn 8)
  (dividers-base-clock-mask #x200)
  (dividers-base-clock-posn 9)
  (dividers-oe-mask #x400)
  (dividers-oe-posn 10))

(defun oga1-dividers-to-u32 (oe base-clock divisor0 divisor1)
  "Combine parameters into a value that can be written to the dividers
register."
  (declare (values (unsigned-byte 32) &optional))
  (logior (if oe +dividers-oe-mask+ 0)
	  (if base-clock +dividers-base-clock-mask+ 0)
	  (logand (ash divisor0 +dividers-divisor0-lsb-posn+)
		  +dividers-divisor0-mask+)
	  (logand (ash divisor1 +dividers-divisor1-lsb-posn+)
		  +dividers-divisor1-mask+)))

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
    "Replaces hyphens with underscores. To convert Lisp names into C
names."
    (dotimes (i (length str) str)
     (when (eq (char str i) #\-)
       (setf (char str i) #\_)))))

(defmacro defc (name ret args &key (card t))
  "Define a foreign function of the liboga1* libraries. Most of them
get card as first parameter. If CARD is nil no such parameter is
defined."
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
#+nil ;; this is the output: 
(:PIXEL-CLOCK-HZ 108090000 :HACT 1280 :HBLANK 408 :VACT 1024 :VBLANK 42
    :HSYNC-OFF 48 :HSYNC-WIDTH 112 :VSYNC-OFF 1 :VSYNC-WIDTH 3 :XSIZE-MM 320
    :YSIZE-MM 240 :HBORDER 0 :VBORDER 0 :INTERLACED 0 :STEREO 0 :SEPARATE-SYNC
    3 :VSYNC-POSITIVE 1 :HSYNC-POSITIVE 1 :STEREO 0)

;; I'm mostly interested in the frequency of 108.09 MHz which will
;; generate 60.07 Hz as shown below:
#+nil
(let ((htotal (+ 1280 408))
      (vtotal (+ 1024 42)))
  (list :htotal htotal
	:vtotal vtotal 
	:vrefresh (/ 108.09e6 (* htotal vtotal))))
;; this returns: (:HTOTAL 1688 :VTOTAL 1066 :VREFRESH 60.069756)

#+nil ;; turn DAC off
(video-reset *card* 1)

;; Now I want to learn how oga1 chooses the right clock frequency
;; This is an example run of oga1-vid-test:
; Requested clock = 216180000
; Actual clock    = 214285680 (difference = 1894320)
; Dividers        = sel:1 pre:35 post:1

;; Apparently it just requested twice the pixel clock.


#+nil
(defparameter *div* (get-clock-dividers *card* (* 2 108090000)))
;; this returns 806, what does that mean?

(defun dividers-u32-to-list (val)
  "Parse the value from the dividers register into its components."
  (list :oe (logand +dividers-oe-mask+ 
		    (ash val (- +dividers-oe-posn+)))
	:base-clock
	(logand +dividers-base-clock-mask+ 
		(ash val (- +dividers-base-clock-posn+)))
	:divisor0
	(logand +dividers-divisor0-mask+ 
		(ash val (- +dividers-divisor0-lsb-posn+)))
	:divisor1
	(logand +dividers-divisor1-mask+ 
		(ash val (- +dividers-divisor1-lsb-posn+)))))

#+nil
(dividers-u32-to-list *div*)
;; 806 -> (:OE 0 :BASE-CLOCK 0 :DIVISOR0 96 :DIVISOR1 6) 

;; as far as I understand this is how the clock is determined,
;; ref-clock is either 133 or 125 MHz 
(defun get-divided-frequency (ref-clock pre post)
  (* 48 (/ ref-clock (ash pre 
			  (1- post)))))

;; the oga1-vid-test chose these parameters
#+nil
(* 1d-6
 (get-divided-frequency +s3-ref-clock1+ 35 1))

;; which gives 171.4 MHz. If divided by the total pixels this results
;; in 47.6 Hz frame rate:
#+nil
(/ 171.4e6 (* 2 1688 1066))


(defun lisp-get-clock-dividers (freq)
  "Find the 4 best clock dividers to achieve frequency."
  (declare (type (unsigned-byte 32) freq))
  (let ((res nil))
    (let ((max-freq (+ freq (/ (* 5 freq) 1000))))
      (dotimes (sel 2)
	(let ((clk (elt (list +s3-ref-clock0+
			      +s3-ref-clock1+)
			sel)))
	  (loop for pre from 1 upto 63 do
	       (when (< +s3-pll-min+ (* 48 (/ clk pre)) +s3-pll-max+)
		 (loop for post from 1 upto 6 do
		      (let ((test-clk (get-divided-frequency clk pre post)))
			(when (<= test-clk max-freq)
			  (let ((diff (abs (- freq test-clk))))
			    (push (list diff (* 1d0 (/ diff test-clk)) sel pre post) res))))))))))
    (subseq (sort res #'< :key #'first)
	    0 4)))
#+nil
(lisp-get-clock-dividers 108090000)

;; the best choice is to use the second reference clock with 125 MHz
;; and pre=28 post=2, this gives a relative frequency error of 0.8 %
#+nil
((6630000/7 0.00884d0 1 28 2) (1426000 0.013369084227105678d0 0 30 2)
 (134610000/29 0.04487d0 1 29 2)
 (150870000/31 0.047148053701342535d0 0 31 2))

;; this is not the same result as oga1_get_clock_dividers returned.
;; it wanted to use the first clock with pre 96 and post 6
;; let's see what the actual frequency of this would be
#+nil
(* 1d-6 (get-divided-frequency +s3-ref-clock0+ 96 6))
;; => 2.0832812499999998d0
;; so this would be 2.08 MHz

(lisp-get-clock-dividers 216180000)



#+nil
(defparameter *f* (get-clock-frequency *card* *div*))
#+nil
(set-video-clock *card* +head-top+ 1 108090000)
;;                                   165000000
#+nil
(dvi-init *card* +head-top+ 108090000) ;; oga1/oga1-utils.c

(define-alien-routine progressive int
  (program (* u32))
  (fb-width u32)
  (fb-height u32)
  (vp-width u32)
  (vp-height u32)
  (depth-bpp u32)
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
			 4 
			 48 112 248
			 1 3 38
			 #x80 2
			 0 1)))
  (lisp-vm-upload 0 program n))

#+nil
(set-video-mode *card* +head-top+ 1 108090000 4)