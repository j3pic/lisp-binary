(defpackage :lisp-binary-doom-wad
  (:use :common-lisp :lisp-binary))

(in-package :lisp-binary-doom-wad)

(defparameter *doom-wad* #P"/usr/share/games/doom/doom.wad")

(define-enum wad-type 1 ()
	     (i 73)
	     (p 80))

(defbinary index-entry (:export t)
  (offset 0 :type (signed-byte 32))
  (size 0 :type (unsigned-byte 32))
  (name "" :type (fixed-length-string 8)))

(defbinary doom-wad-header (:export t)
  (wad-type 0 :type wad-type)
  (magic 0 :type (magic :actual-type (unsigned-byte 24)
			:value #x444157))
  (num-index-entries 0 :type (signed-byte 32))
  (index-offset 0 :type (signed-byte 32))
  (index-entries nil :type null))

(defbinary thing (:export t)
  (x 0 :type (signed-byte 16))
  (y 0 :type (signed-byte 16))
  (angle 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 16))
  ((easy medium hard deaf multiplayer-only
	 not-in-deathmatch
	 not-in-coop reserved) 0 :type (bit-field :raw-type (unsigned-byte 16)
					 :member-types ((unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 1)
							(unsigned-byte 9)))))


(defbinary linedef (:export t)
  (start-vertex-ix 0 :type (unsigned-byte 32))
  (end-vertex-ix 0 :type (unsigned-byte 32))
  ((impassable blocks-monsters two-sided
	       upper-texture-unpegged lower-texture-unpegged
	       secret blocks-sound  never-map
	       ;; FIXME: The fields below should really
	       ;;        be one field, five bits wide,
	       ;;        that form an ENUM. But the library
	       ;;        doesn't support using ENUMs in bit fields.
	       ;; As written, there are more fields than there are
	       ;; sizes.
	         always-map ;; 0x1
	         multi-activatable ;; 0x2
		 activate-on-use ;; 0x4
	         activate-on-monster-crossing ;; 0x8
		 activate-on-shooting ;; 0xc
	         activate-on-player-bump ;; 0x10
		 activate-on-projectile-crossing ;; 0x14
	         activate-on-use-with-passthrough ;; 0x18
	       ;; 
	       activatable-by-players-and-monsters
	       reserved-1 blocks-everything reserved-2)
    0
   :type  (bit-field :raw-type
				  (unsigned-byte 16)
				  :member-types ((unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1)
						 (unsigned-byte 1))))
  (type 0 :type (unsigned-byte 16))
  (sector-tag 0 :type (unsigned-byte 16))
  (left-sidedef-ix 0 :type  (unsigned-byte 16))
  (right-sidedef-ix 0 :type (unsigned-byte 16)))



(defun read-indices (stream offset num)
  (let ((result (make-array num :element-type 'index-entry))
	(old-file-position (file-position stream)))
    (unwind-protect
	 (progn
	   (file-position stream offset)
	   (loop for ix from 0 below num
	      do (setf (aref result ix) (read-binary 'index-entry stream))))
      (file-position stream old-file-position))
    result))

(defun ad-hoc-read-wad (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (let* ((result (read-binary 'doom-wad-header in)))
      (setf (slot-value result 'index-entries)
	    (read-indices in (slot-value result 'index-offset)
			  (slot-value result 'num-index-entries)))
      result)))
	   
