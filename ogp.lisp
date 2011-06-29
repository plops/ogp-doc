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
  ;; block xp10
  (xp10-video-reset #x20)
  (xp10-interrupt-status #x24)
  (xp10-interrupt-clear #x24)
  (xp10-dac-power-on #x28)
  (xp10-interrupt-mask #x30) ;; rw
  (xp10-test-reg #x7c) ;; rw
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
  (dividers-oe-posn 10)
  ;;
  (vid-fb-offset-t #x8000000)
  )

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

;; user
(load-shared-object "/opt/ogp/lib/liboga1-user.so")

(defc card-open card ((bus int) (dev int) (func int)) :card nil)
(defc card-free void nil)
(defc card-set-log-file void ((file-pointer (* int))))
(defc edid-dump void ((file-pointer (* int))
		      (ddc (* u8))))

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

(defun dividers-to-u32 (oe base-clock divisor0 divisor1)
  "Combine parameters into a value that can be written to the dividers
register."
  (declare (type (unsigned-byte 1) oe base-clock)
	   (type (unsigned-byte 3) divisor1) 
	   (type (unsigned-byte 6) divisor0) 
	   (values (unsigned-byte 32) &optional))
  (let ((res 0))
    (setf (ldb (byte 1 +dividers-oe-posn+) res) oe
	  (ldb (byte 1 +dividers-base-clock-posn+) res) base-clock
	  (ldb (byte 3 +dividers-divisor1-lsb-posn+) res) divisor1
	  (ldb (byte 6 +dividers-divisor0-lsb-posn+) res) divisor0)
    res))

(defun dividers-u32-to-list (v)
  "Parse the value from the dividers register into its components."
  (declare (type (unsigned-byte 32) v))
  (list :oe (ldb (byte 1 +dividers-oe-posn+) v)
	:base-clock (ldb (byte 1 +dividers-base-clock-posn+) v)
	:divisor0 (ldb (byte 6 +dividers-divisor0-lsb-posn+) v)
	:divisor1 (ldb (byte 3 +dividers-divisor1-lsb-posn+) v)))

(defun get-divided-frequency (ref-clock pre post)
  (* 48 (/ ref-clock (ash pre 
			  (1- post)))))

(defun lisp-get-clock-dividers (freq)
  "Find the 4 best clock divider combinations to achieve frequency."
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

(defun lisp-vm-upload (base program n)
  (declare (type (unsigned-byte 32) base n)
	   (type (simple-array (unsigned-byte 32) (512)) program))
 (let ((addr +vm0-program-memory+))
   (incf addr (* 4 base))
   (dotimes (i n)
     (card-rset *card* addr (aref program i))
     (incf addr 4))))

(defun lisp-set-video-clock ()
  (card-rset *card* +vc0-dividers+ (dividers-to-u32 0 1 28 1))
  (sleep .001)
  (card-rset *card* +vc0-dividers+ (dividers-to-u32 1 1 28 1))
  (sleep .001))

(defun lisp-set-pixel-info (depth rate)
  (declare (type (unsigned-byte 3) depth)
	   (type (unsigned-byte 1) rate))
  (let ((v 0))
    (declare (type (unsigned-byte 32) v))
    (setf (ldb (byte 1 3) v) rate
	  (ldb (byte 3 0) v) depth)
    (card-rset *card* +vc0-pixel-info+ v)
    v))

(defun lisp-set-output-mode (digital not-dvi-sl dac-de-en dac-sync-en
			     vsync-polarity hsync-polarity)
  (declare (type (unsigned-byte 1) digital not-dvi-sl dac-de-en dac-sync-en
		 vsync-polarity hsync-polarity))
  (let ((v 0))
    (declare (type (unsigned-byte 32) v))
    (setf (ldb (byte 1 0) v) digital
	  (ldb (byte 1 1) v) not-dvi-sl
	  (ldb (byte 1 2) v) dac-de-en
	  (ldb (byte 1 3) v) dac-sync-en
	  (ldb (byte 1 4) v) vsync-polarity
	  (ldb (byte 1 5) v) hsync-polarity)
    (card-rset *card* +vc0-output-mode+ v)
    v))

(defun lisp-unset-video-mode ()
  (card-rset *card* +vc0-cpu-reset-b+ 0)  ;; disable video program counter
  (card-rset *card* +vc0-video-enable+ 0) ;; disable video controller
  (card-rset *card* +vc0-pc-start+ 0)     ;; reset program counter
  (card-rset *card* +vc0-interrupt-enable+ 0))

(defun lisp-set-video-mode ()
  (lisp-set-output-mode 1 0 0 0 1 0)
  (sleep .001)
  (lisp-unset-video-mode)
  (let ((dvi-sl 1)
	(vid-mode 5))
    (lisp-set-pixel-info vid-mode dvi-sl))
  
  (lisp-set-output-mode 1 0 0 0 0 1)
  (card-rset *card* +vc0-cpu-reset-b+ 1)
  (card-rset *card* +vc0-video-enable+ 1)
  (card-rset *card* +vc0-clear-int+ 1)
  (card-rset *card* +xp10-interrupt-mask+ #b111)
  (sleep .001)
  (card-rset *card* +vc0-interrupt-enable+ 1))

#+nil
(card-rset *card* +xp10-interrupt-mask+ #b111)
#+nil
(card-rget *card* +xp10-interrupt-mask+)
#+nil
(card-rset *card* +vc0-interrupt-enable+ 1)
#+nil
(card-rset *card* +vc0-video-enable+ 1)
#+nil
(card-rset *card* +vc0-clear-int+ 1)
#+nil
(card-rget *card* +xp10-interrupt-status+)
#+nil
(progn
 (card-rset *card* +xp10-interrupt-clear+ 0)
 (card-rget *card* +xp10-interrupt-status+))


(defun lisp-set-mode ()
  (lisp-set-video-clock)
  (dvi-init *card* +head-top+ 108090000)

  (let* ((program (make-array 512 :element-type '(unsigned-byte 32)))
	 (sap (sb-sys:vector-sap program))
	 (n   (progressive sap
			   1280 1024
			   1280 1024
			   4 
			   48 112 248
			   1 3 38
			   #x80 2
			   0 0)))
    (lisp-vm-upload 0 program n)) 
  
  (lisp-set-video-mode))

#+nil
(defparameter *card* (card-open #x0b 1 0))
#+nil
(card-mem-init *card*)
#+nil
(let ((ddc (make-array 128 :element-type '(unsigned-byte 8))))
 (defparameter *top* (i2c-get-edid *card* +head-top+ (sb-sys:vector-sap ddc)))
 ;; 0 success, 1 timeout, 2 bad head, 3 bad header, 4 bad checksum
 (defparameter *ddc* ddc)
 (parse-ddc))

(defun lisp-video-reset ()
  (card-rset *card* +xp10-video-reset+ 0)
  (sleep .001)
  (card-rset *card* +xp10-video-reset+ 1)
  (card-rset *card* +xp10-dac-power-on+ 0))

#+nil ;; turn DAC off
(lisp-video-reset)

#+nil
(lisp-set-mode)

#+nil
(lisp-unset-video-mode)

#+nil
(card-mget *card* +vid-fb-offset-t+)


#+nil
(card-mset *card* +vid-fb-offset-t+ #x00123456)
#+nil
(card-mget *card* +vid-fb-offset-t+)

(defun draw-screen ()
  (let ((pitch 1280))
   (dotimes (i 1280)
     (dotimes (j 1024)
       (declare (type (unsigned-byte 16) j i pitch))
       (let ((addr (+ +vid-fb-offset-t+
		      (* 4 (+ (* pitch j)
			      i)))))
	 (card-mset *card* addr #xfaffaaff))))))
#+nil
(time
 (draw-screen))


(defmacro def-choice (name choices)
  `(defun ,name (&optional (param ,(first (first choices))))
     (declare (type (member ,@(mapcar #'first choices)) param))
     (ecase param ,@choices)))

(def-choice cursor-to-nr ((:no-change #b0)
		    (:advance-x #b01)
		    (:reset-x-advance-y #b10)
		    (:reset-xy #b11)))

(def-choice opcode-to-nr ((:jump 0)
		    (:call 1)
		    (:wait 2)
		    (:send 3)
		    (:fetch 5)
		    (:addr 6)
		    (:inc 7)))

(defun vc (opcode &key (de 0) (vsync 0) (hsync 0)
		    (cursor :no-change) (ret 0) (int 0) 
		    (count 0 count-p) (iaddr 0 iaddr-p) 
		    (memory-address 0 memory-address-p))
  "Construct an instruction for the video controller."
  (declare (type (unsigned-byte 1) de vsync hsync ret int)
	   (TYPE (MEMBER :NO-CHANGE :ADVANCE-X
			 :RESET-X-ADVANCE-Y :RESET-XY) cursor)
	   (TYPE (MEMBER :JUMP :CALL :WAIT :SEND 
			 :FETCH :ADDR :INC) opcode)
	   (type (unsigned-byte 12) count)
	   (type (unsigned-byte 9) iaddr)
	   (type (unsigned-byte 21) memory-address)
	   (values (unsigned-byte 32) &optional))
  (when (and count-p memory-address-p)
    (break "count and memory-address given at the same time."))
  (when (and iaddr-p memory-address-p)
    (break "iaddr and memory-address given at the same time."))
  
  (let ((v 0)
	(opcode-nr (opcode-to-nr opcode))
	(cursor-nr (cursor-to-nr cursor)))
    (declare (type (unsigned-byte 3) opcode-nr)
	     (type (unsigned-byte 2) cursor-nr))
    (when (member opcode '(:call :wait :send))
      (setf count (- 2049 count)))
    (setf (ldb (byte 1 31) v) de
	  (ldb (byte 1 30) v) vsync 
	  (ldb (byte 1 29) v) hsync
	  (ldb (byte 2 27) v) cursor-nr
	  (ldb (byte 1 26) v) ret
	  (ldb (byte 1 25) v) int
	  (ldb (byte 3 21) v) opcode-nr)
    (when (or count-p iaddr-p)
      (setf (ldb (byte 12 9) v) count
	    (ldb (byte 9 0) v) iaddr))
    (when memory-address-p
      (setf (ldb (byte 21 0) v) memory-address))
    v))


(let ((vfp 2)
      (vsync 7)
      (vbp 3))
 (list
  (vc :call :count vfp :iaddr 22 :hsync 1)
  (vc :call :count vsync :iaddr 26 :hsync 1 :vsync 1)
  (vc :call :count (1- vbp) :iaddr 22 :hsync 1 :vsync 1)
  (vc :addr)))