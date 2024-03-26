;;; components.lisp --- wheel components

;; Copyright (C) 2024 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :spoke-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

;;;; Data Types

(defclass component ()
  ((component-name
    :documentation "A short description of the component."
    :accessor component-name
    :initarg :name
    :initform nil
    :type (or null string))))

(defmethod initialize-instance :after ((object component) &key)
  (iter (with class = (class-of object))
	(for slot :in (closer-mop:class-slots class))
	(for slot-name = (closer-mop:slot-definition-name slot))
	(when (slot-boundp object slot-name)
	  (for slot-value = (slot-value object slot-name))
	  ;; Check that the contents of the slot is of the specified
	  ;; data type.
	  (for slot-type = (closer-mop:slot-definition-type slot))
	  (unless (typep slot-value slot-type)
	    (error 'type-error :datum slot-value :expected-type slot-type)))))

(defclass axle (component)
  ((diameter
    :documentation "The diameter of the wheel axle."
    :accessor axle-diameter
    :initarg :diameter
    :initform 0
    :type (real 0))
   (length
    :documentation "The over locknut distance of the wheel axle, i.e. the distance between the dropouts."
    :accessor axle-length
    :initarg :length
    :initform 0
    :type (real 0))))

(defclass thru-axle (axle)
  ((thread
    :documentation "The thread specification."
    :accessor axle-thread
    :initarg :thread
    :initform nil
    :type (or null string))))

(defclass hub (component)
  ((pitch-circle-diameter
    :documentation "The pitch circle diameter of the spoke holes on the flange."
    :accessor pitch-circle-diameter
    :initarg :pitch-circle-diameter
    :initform 0
    :type (or (real 0) (cons (real 0) (real 0))))
   (pitch-circle-distance
    :documentation "The distance from the middle of the hub to the pitch circle."
    :accessor pitch-circle-distance
    :initarg :pitch-circle-distance
    :initform 0
    :type (or (real 0) (cons (real 0) (real 0))))
   (spoke-hole-diameter
    :documentation "The diameter of the spoke holes."
    :accessor spoke-hole-diameter
    :initarg :spoke-hole-diameter
    :initform 2.5
    :type (real 0))
   ;; Metadata.
   (wheel-axle
    :documentation "The wheel axle standard."
    :accessor wheel-axle
    :initarg :wheel-axle
    :initform nil
    :type (or null axle))
   ;; Measurements.
   (axle-length
    :documentation "The over locknut distance of the wheel axle, i.e. the distance between the dropouts."
    :accessor axle-length
    :initarg :axle-length
    :initform nil
    :type (or null (real 0)))
   (flange-distance
    :documentation "The distance from the flange to the adjacent dropout."
    :accessor flange-distance
    :initarg :flange-distance
    :initform nil
    :type (or null (real 0) (cons (real 0) (real 0))))
   (flange-thickness
    :documentation "The thickness of the flange at the pitch circle diameter."
    :accessor flange-thickness
    :initarg :flange-thickness
    :initform nil
    :type (or null (real 0) (cons (real 0) (real 0))))))

(defclass straight-pull-hub (hub)
  ((spoke-hole-offset
    :documentation "The spoke length correction for straight pull hubs."
    :accessor spoke-hole-offset
    :initarg :spoke-hole-offset
    :initform 0
    :type (or real (cons real real)))))

(defclass rim (component)
  ((base-diameter
    :documentation "The base diameter, i.e. pitch circle diameter, of the spoke holes on the rim."
    :accessor base-diameter
    :initarg :base-diameter
    :initform 0
    :type (real 0))
   (pitch-circle-distance
    :documentation "The distance from the middle of the rim to the pitch circle."
    :accessor pitch-circle-distance
    :initarg :pitch-circle-distance
    :initarg :offset
    :initform 0
    :type (real 0))
   (first-spoke-hole
    :documentation "The orientation of the first spoke hole."
    :accessor first-spoke-hole
    :initarg :first-spoke-hole
    :initform :center
    :type (member :left :center :right))
   ;; Measurements.
   (inner-diameter
    :documentation "The inner diameter of the rim."
    :accessor inner-diameter
    :initarg :inner-diameter
    :initform nil
    :type (or null (real 0)))
   (thickness
    :documentation "The thickness of the rim at the spoke holes."
    :accessor thickness
    :initarg :thickness
    :initform nil
    :type (or null (real 0)))))

(defclass spoke (component)
  ((diameter
    :documentation "The diameter of the spoke at the head."
    :accessor spoke-diameter
    :initarg :diameter
    :initform 2.0
    :type (real 0))
   (area
    :documentation "The cross-sectional area of the spoke on the middle section."
    :accessor spoke-area
    :initarg :area
    :initform 0
    :type (real 0))
   (strength
    :documentation "The strength of the spoke on the middle section."
    :accessor spoke-strength
    :initarg :strength
    :initform 0
    :type (real 0))
   (material
    :documentation "The material identifier."
    :accessor spoke-material
    :initarg :material
    :initform nil
    :type (or string symbol))
   (middle-section
    :documentation "The length of the middle section."
    :accessor spoke-middle-section
    :initarg :middle-section
    :initform 0
    :type (or real function))
   (thread
    :documentation "The thread specification."
    :accessor spoke-thread
    :initarg :thread
    :initform "14G"
    :type (or string symbol))))

(defclass nipple (component)
  ((length
    :documentation "The length of the nipple."
    :accessor nipple-length
    :initarg :length
    :initform 12.0
    :type (real 0))
   (head-height
    :documentation "The height of the nipple head."
    :accessor nipple-head-height
    :initarg :head-height
    :initform 3.0
    :type (real 0))))

(defclass washer (component)
  ((thickness
    :documentation "The thickness of the washer."
    :accessor washer-thickness
    :initarg :thickness
    :initform 0
    :type (real 0))))

;;;; Utilities

(defun make-hub-from-measurements (class &rest arguments &key axle-length flange-distance flange-thickness &allow-other-keys)
  "Create a hub object from measured values.

First argument CLASS is the class of the hub.  Value is a symbol and
 must be a sub-type of ‘hub’.
Keyword argument AXLE-LENGTH is the installation width of the hub,
 i.e. the distance between the dropouts.
Keyword argument FLANGE-DISTANCE is the distance from the flange to
 the adjacent dropout.
Keyword argument FLANGE-THICKNESS is the thickness of the flanges at
 the pitch circle diameter.
Remaining arguments are initialization arguments of CLASS.

Return value is a hub object."
  (unless (subtypep class 'hub)
    (error 'type-error :datum class :expected-type 'hub))
  (when (null axle-length)
    (when-let ((axle (getf arguments :wheel-axle)))
      (setf axle-length (axle-length axle))))
  (check-type axle-length (real 0))
  (check-type flange-distance (or (real 0) (cons (real 0) (real 0))))
  (check-type flange-thickness (or (real 0) (cons (real 0) (real 0))))
  (let ((flange-distance-left (left flange-distance))
        (flange-distance-right (right flange-distance))
        (flange-thickness-left (left flange-thickness))
        (flange-thickness-right (right flange-thickness)))
    (apply #'make-instance class
           :axle-length axle-length
           :flange-distance flange-distance 
           :flange-thickness flange-thickness
           :pitch-circle-distance (cons (- (/ axle-length 2) flange-distance-left (/ flange-thickness-left 2))
                                        (- (/ axle-length 2) flange-distance-right (/ flange-thickness-right 2)))
           arguments)))

(defun make-rim-from-measurements (class &rest arguments &key inner-diameter thickness &allow-other-keys)
  "Create a rim object from measured values.

First argument CLASS is the class of the rim.  Value is a symbol and
 must be a sub-type of ‘rim’.
Keyword argument INNER-DIAMETER is the inner diameter of the rim.
 If the spoke holes have eyelets, the height of the eyelets has to
 be excluded.
Keyword argument THICKNESS is the thickness of the rim at the spoke
 holes.  If the spoke holes have eyelets, the outer height of the
 eyelets has to be included.
Remaining arguments are initialization arguments of CLASS.

Return value is a rim object."
  (unless (subtypep class 'rim)
    (error 'type-error :datum class :expected-type 'rim))
  (check-type inner-diameter (real 0))
  (check-type thickness (real 0))
  (apply #'make-instance class
         :inner-diameter inner-diameter
         :thickness thickness
         :base-diameter (+ inner-diameter (* 2 thickness))
         arguments))

;;; components.lisp ends here
