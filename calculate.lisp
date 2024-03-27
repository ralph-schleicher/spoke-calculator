;;; calculate.lisp --- a spoke calculator

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

;;; Commentary:

;; https://www.sapim.be/spoke-calculator
;; https://spokes-calculator.dtswiss.com/de/calculator
;; https://www.sheldonbrown.com/rinard/spocalc.htm
;; https://kstoerz.com/freespoke/
;; http://www.spokecalculator.net/

;;; Code:

(in-package :spoke-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

(deftype total-number-of-spokes ()
  '(member 20 24 28 32 36))

(defconst +left+ -1)
(defconst +right+ +1)
(defconst +radial+ 0)
(defconst +trailing+ -1)
(defconst +leading+ +1)

(defsubst leftp (number)
  (minusp number))

(defsubst rightp (number)
  (plusp number))

(defsubst radialp (number)
  (zerop number))

(defsubst trailingp (number)
  (minusp number))

(defsubst leadingp (number)
  (plusp number))

(defparameter *default-spoke*
  (make-instance 'spoke
                 :diameter 2.0
                 :area (area-of-circle 2.0)
                 :thread "14G")
  "The default spoke.")

(defparameter *default-nipple*
  (make-instance 'nipple
                 :length 12.0
                 :head-height 3.0)
  "The default nipple.")

(defvar *data* nil
  "The latest result data set.")

(defun calculate (&key
                    hub rim washer spoke nipple
                    (spoke-count 32)
                    (spoke-crossings 3)
                    (spoke-tension 120.0)
                    (spoke-tension-method :max)
                    (rim-offset-orientation :auto)
                    (nipple-head-extent 2.0))
  "Calculate the spoke lengths for a wheel assembly.

Keyword arguments HUB, RIM, WASHER, SPOKE, and NIPPLE define the
 components of the wheel assembly.  Only HUB and RIM are mandatory.
 If WASHER is a number, it defines the thickness of the washer in
 millimeter.
Keyword argument SPOKE-COUNT is the number of spokes.  Value is
 either 20, 24, 28, 32, or 36.  Default is 32.
Keyword argument SPOKE-CROSSINGS is the number of spoke crossings.
 Value is either 0 (radial), 1, 2, 3, or 4.  Default is 3.
Keyword argument SPOKE-TENSION is the spoke tension in kilogram
 when truing the wheel.  Value has to be a non-negative number.
 Default is 120 kilogram.  The spoke tension is used to estimate
 the elongation of the spokes.  This information can be used to
 adjust the actual spoke length.
Keyword argument SPOKE-TENSION-METHOD defines how to apply the
 SPOKE-TENSION parameter.  Value is either ‘:max’, ‘:min’, or
 ‘:mean’ to apply the spoke tension as the maximum, minimum,
 or mean spoke tension respectively.  Default is ‘:max’.
Keyword argument RIM-OFFSET-ORIENTATION defines the orientation of
 an asymmetric rim.  Value is either ‘:left’, ‘:right’, or ‘:auto’.
 Default is ‘:auto’.  A value of ‘:left’ means, that the spoke holes
 are to the left of the middle of the rim.  A value of ‘:right’ means,
 that the the spoke holes are to the right of the middle of the rim.
 A value of ‘:auto’ means, that the rim orientation is selected in
 such a way that the tension ratio is improved.
Keyword argument NIPPLE-HEAD-EXTENT is the additional spoke length in
 millimeter that extends from the rim (washer) contact point into the
 nipple head.  Value has to be a number.  Default is 2 millimeter.
 DT Swiss uses 2 millimeter for their ERD calculation.

The value of the SPOKE, NIPPLE, SPOKE-COUNT, and SPOKE-CROSSINGS
argument is either an object as documented above or a cons cell where
the ‘car’ denotes the left-hand side object and the ‘cdr’ denotes the
right-hand side object.  The SPOKE-COUNT and SPOKE-CROSSINGS values
define the spoke's lacing pattern.

See also ‘hub’, ‘rim’, ‘washer’, ‘spoke’, and ‘nipple’."
  (check-type hub hub)
  (check-type rim rim)
  (check-type washer (or null washer (real 0)))
  (check-type spoke (or null spoke (cons spoke spoke)))
  (check-type nipple (or null nipple (cons nipple nipple)))
  (check-type spoke-count (or total-number-of-spokes (cons (integer (0)) (integer (0)))))
  (check-type spoke-crossings (or (integer 0 4) (cons (integer 0 4) (integer 0 4))))
  (check-type spoke-tension (real 0))
  (check-type spoke-tension-method (member :max :min :mean))
  (check-type rim-offset-orientation (member :left :auto :right))
  (check-type nipple-head-extent real)
  (when (null spoke)
    (setf spoke *default-spoke*))
  (when (null nipple)
    (setf nipple *default-nipple*))
  (if (consp spoke-count)
      ;; Check total number of spokes.
      (let ((left (left spoke-count))
            (right (right spoke-count)))
        (unless (and (multiple-value-bind (quotient remainder)
                         (truncate (max left right) (min left right))
                       (and (or (= quotient 1) (= quotient 2)) (= remainder 0)))
                     (typep (+ left right) 'total-number-of-spokes))
          (error 'type-error :datum spoke-count :expected-type 'total-number-of-spokes)))
    ;; Split total number of spokes.
    (let ((left (zerop (left spoke-crossings)))
          (right (zerop (right spoke-crossings))))
      (multiple-value-bind (quotient remainder)
          (truncate spoke-count (+ (if left 1 2) (if right 1 2)))
        (unless (zerop remainder)
          (error 'type-error :datum spoke-count :expected-type 'total-number-of-spokes))
        (setf spoke-count (cons (if left quotient (* 2 quotient))
                                (if right quotient (* 2 quotient)))))))
  (let* (;; Pitch circle diameter of the spoke holes on the hub.
         (hub-diameter (cons (left (pitch-circle-diameter hub))
                             (right (pitch-circle-diameter hub))))
         ;; Pitch circle radius of the spoke holes on the hub.
         (hub-radius (cons (/ (%left hub-diameter) 2)
                           (/ (%right hub-diameter) 2)))
         ;; Distance from the middle of the hub to the middle of the
         ;; flange.  Difference in length of outer and inner spoke is
         ;; neglected.
         (hub-distance (cons (left (pitch-circle-distance hub))
                             (right (pitch-circle-distance hub))))
         ;; Thickness of the washer.
         (washer-thickness (etypecase washer
                             (washer
                              (washer-thickness washer))
                             (real
                              washer)
                             (null
                              0)))
         ;; Pitch circle radius of the spoke holes on the rim.
         (rim-radius (+ (/ (base-diameter rim) 2) washer-thickness))
         ;; Offset of the rim's pitch circle from the middle of the rim.
         (rim-offset (let ((rim-offset (pitch-circle-distance rim)))
                       (unless (zerop rim-offset)
                         (ecase rim-offset-orientation
                           (:left
                            (setf rim-offset (- rim-offset)))
                           (:auto
                            ;; Calculate the bracing angle difference
                            ;; when the rim offset is either on the
                            ;; non-gear or gear side.  The smaller
                            ;; value has the better tension ratio.
                            (let ((left (abs (- (atan (- (%left hub-distance) rim-offset)
                                                      (- rim-radius (%left hub-radius)))
                                                (atan (+ (%right hub-distance) rim-offset)
                                                      (- rim-radius (%right hub-radius))))))
                                  (right (abs (- (atan (+ (%left hub-distance) rim-offset)
                                                       (- rim-radius (%left hub-radius)))
                                                 (atan (- (%right hub-distance) rim-offset)
                                                       (- rim-radius (%right hub-radius)))))))
                              (when (< left right)
                                (setf rim-offset (- rim-offset)))))
                           (:right)))
                       rim-offset))
         ;; Like ‘hub-distance’ but corrected by the rim offset.
         (hub-distance* (if (zerop rim-offset)
                            hub-distance
                          (cons (+ (%left hub-distance) rim-offset)
                                (- (%right hub-distance) rim-offset))))
         ;; Number of spoke crossings.
         (crossings (cons (left spoke-crossings)
                          (right spoke-crossings)))
         ;; Whether or not the wheel is radially spoked.
         (radialp (cons (zerop (%left crossings))
                        (zerop (%right crossings))))
         ;; Number of spoke holes on the hub.
         (hub-holes spoke-count)
         ;; Number of spoke holes on the rim.
         (rim-holes (+ (%left hub-holes) (%right hub-holes)))
         ;; Central angle of a hub sector.
         (hub-angle (cons (/ 360 (%left hub-holes))
                          (/ 360 (%right hub-holes))))
         ;; Central angle of a rim sector.
         (rim-angle (/ 360 rim-holes))
         ;; Number of spoke holes (step size) for similar spokes on the hub.
         (hub-step (cons (if (%left radialp) 1 2)
                         (if (%right radialp) 1 2)))
         ;; Number of spoke holes (step size) for similar spokes on the rim.
         (rim-step (+ (%left hub-step) (%right hub-step)))
         ;; Number of spoke holes (step size) for a pair of leading
         ;; and trailing spokes on the rim.
         (rim-pair (cons (if (%left radialp) 0 (- (* (%left crossings) rim-step) (%right hub-step)))
                         (if (%right radialp) 0 (- (* (%right crossings) rim-step) (%left hub-step)))))
         ;; Spoke hole coordinates.
         ;;
         ;; The hub's center (middle of the spin axle's centerline) is
         ;; at the origin of the coordinate system.
         ;; Spokes are numbered zero-based in clockwise direction in
         ;; the order of the spoke holes on the rim.
         ;; The valve hole is between the first and last spoke hole.
         ;; The angle of the valve hole is 90° in the (x, y) plane.
         (start-index (cond ((and (%left radialp) (not (%right radialp)))
                             ;; Triplet lacing.  Orientation of the spoke
                             ;; holes can be ignored.  The crossed spokes
                             ;; must start at the first spoke hole.
                             ;; Otherwise, the start angle of the first
                             ;; spoke hole on the hub is wrong.
                             (cons 2 0))
                            ((and (%right radialp) (not (%left radialp)))
                             (cons 0 2))
                            ;; Both sides radial or both sides crossed.
                            ((not (eq (first-spoke-hole rim) :left))
                             (cons 1 0))
                            (t
                             (cons 0 1))))
         (valve-hole (let ((array (make-array (list 3) :initial-element 0)))
                       (setf (aref array 0) 0.0
                             (aref array 1) rim-radius
                             (aref array 2) rim-offset)
                       array))
         (rim-hole (let ((array (make-array (list rim-holes 3) :initial-element 0)))
                     (iter (for index :from 0 :below rim-holes)
                           (for angle :from (- 90 (/ rim-angle 2)) :by (- rim-angle)) ;clockwise
                           (setf (aref array index 0) (* rim-radius (cosd (float angle pi)))
                                 (aref array index 1) (* rim-radius (sind (float angle pi)))
                                 (aref array index 2) rim-offset))
                     array))
         ;; The ‘spoke-pattern’ function creates the array.
         (hub-hole (spoke-pattern (spoke-pattern nil
                                                 (%left start-index)
                                                 (%left crossings)
                                                 rim-holes
                                                 (%left hub-holes)
                                                 (%left hub-radius)
                                                 (- (%left hub-distance))
                                                 +left+)
                                  (%right start-index)
                                  (%right crossings)
                                  rim-holes
                                  (%right hub-holes)
                                  (%right hub-radius)
                                  (+ (%right hub-distance))
                                  +right+))
         ;; Geometric spoke length (keep the maths as a reference for
         ;; the sanity check).
         (spoke-distance (cons (if (%left radialp)
                                   (hypot (%left hub-distance*)
                                          (- rim-radius (%left hub-radius)))
                                 ;; Combine the cosine theorem and the
                                 ;; Pythagorean theorem to calculate the
                                 ;; square root only once.
                                 (sqrt (- (+ (expt (%left hub-distance*) 2)
                                             (expt (%left hub-radius) 2)
                                             (expt rim-radius 2))
                                          (* 2 (%left hub-radius) rim-radius
                                             (cosd (if (%right radialp) ;triplet lacing
                                                       (- (* (+ (%left crossings) 1/2) (%left hub-angle)) rim-angle)
                                                     (* (%left crossings) (%left hub-angle))))))))
                               (if (%right radialp)
                                   (hypot (%right hub-distance*)
                                          (- rim-radius (%right hub-radius)))
                                 (sqrt (- (+ (expt (%right hub-distance*) 2)
                                             (expt (%right hub-radius) 2)
                                             (expt rim-radius 2))
                                          (* 2 (%right hub-radius) rim-radius
                                             (cosd (if (%left radialp)
                                                       (- (* (+ (%right crossings) 1/2) (%right hub-angle)) rim-angle)
                                                     (* (%right crossings) (%right hub-angle))))))))))
         ;; Angle between the spokes' tension force and the (x, y) plane.
         ;; Mnemonic: α is the angle of attack in flight dynamics.
         (sin-alpha (cons (/ (%left hub-distance*) (%left spoke-distance))
                          (/ (%right hub-distance*) (%right spoke-distance))))
         (alpha (cons (degree-from-radian (asin (%left sin-alpha)))
                      (degree-from-radian (asin (%right sin-alpha)))))
         ;; Tension ratio (the lateral forces in the triange of forces are equal).
         (tension-ratio (cons (/ (%right sin-alpha) (%left sin-alpha))
                              (/ (%left sin-alpha) (%right sin-alpha))))
         ;; Spoke length excluding the nipple head extent.
         ;;
         ;; Subtract half of the spoke diameter Dₛ since the spoke length
         ;;  is measured from the inside of the bend to the tip.
         ;; Subtract half of the play of the spoke hole Dₕ.
         ;;
         ;; L* = L − Dₛ / 2 − (Dₕ − Dₛ) / 2 = L − Dₕ / 2
         ;;
         ;; Straight pull: Add the spoke hole offset.
         (spoke-length (let ((offset (typecase hub
                                       (straight-pull-hub
                                        (spoke-hole-offset hub))
                                       (t
                                        (- (/ (spoke-hole-diameter hub) 2))))))
                         (cons (+ (%left spoke-distance) offset)
                               (+ (%right spoke-distance) offset))))
         ;; Spoke tension.
         (tension (let ((left (%left tension-ratio))
                        (right (%right tension-ratio)))
                    (ecase spoke-tension-method
                      (:max
                       (if (< left right)
                           (cons (* left spoke-tension) spoke-tension)
                         (cons spoke-tension (* right spoke-tension))))
                      (:min
                       (if (> left right)
                           (cons (* left spoke-tension) spoke-tension)
                         (cons spoke-tension (* right spoke-tension))))
                      (:mean
                       (let* ((x (min left right))
                              (high (/ (* 2 spoke-tension) (1+ x)))
                              (low (* x high)))
                         (if (< left right)
                             (cons low high)
                           (cons high low)))))))
         ;; Spoke elongation with a nipple head extent of 2 mm.
         (spoke-elongation (let ((extent nipple-head-extent))
                             (cons (spoke-elongation (left spoke) (+ (%left spoke-length) extent) (%left tension))
                                   (spoke-elongation (right spoke) (+ (%right spoke-length) extent) (%right tension)))))
         ;; Minimum spoke length with half the spoke elongation and
         ;; a nipple head extent of 1 mm (so that the thread is no
         ;; longer visible).
         (minimum-length (let ((extent 1.0))
                           (cons (round-up (- (+ (%left spoke-length) extent) (/ (%left spoke-elongation) 2)) 1/10)
                                 (round-up (- (+ (%right spoke-length) extent) (/ (%right spoke-elongation) 2)) 1/10))))
         ;; Maximum spoke length with the full spoke elongation and
         ;; a nipple head extent of 3.5 mm (so there is still 0.5 mm,
         ;; i.e. one round, until the thread blocks).
         (maximum-length (let ((extent 3.5))
                           (cons (round-down (- (+ (%left spoke-length) extent) (%left spoke-elongation)) 1/10)
                                 (round-down (- (+ (%right spoke-length) extent) (%right spoke-elongation)) 1/10)))))
    ;; Sanity check for the spoke pattern.
    (let ((epsilon (* 4 double-float-epsilon)))
      (iter (for index :from 0 :below rim-holes)
            (for ref = (if (leftp (aref hub-hole index 3))
                           (%left spoke-distance)
                         (%right spoke-distance)))
            (for length = (hypot3 (- (aref hub-hole index 0)
                                     (aref rim-hole index 0))
                                  (- (aref hub-hole index 1)
                                     (aref rim-hole index 1))
                                  (- (aref hub-hole index 2)
                                     (aref rim-hole index 2))))
            (for err = (abs (- 1.0 (/ ref length))))
            (unless (< err epsilon)
              (error "Length of spoke #~A is wrong, ~A ≠ ~A, rel. error ~A." index ref length err))))
    ;; Return value.
    (setf *data* (nconc
                  (when-let ((axle (wheel-axle hub)))
                    (list :axle-label (component-name axle)
                          :axle-diameter (axle-diameter axle)
                          :axle-radius (/ (axle-diameter axle) 2)
                          :axle-length (axle-length axle)))
                  (list :hub-label (component-name hub)
                        :hub-diameter-left (%left hub-diameter)
                        :hub-diameter-right (%right hub-diameter)
                        :hub-radius-left (%left hub-radius)
                        :hub-radius-right (%right hub-radius)
                        :hub-distance-left (%left hub-distance)
                        :hub-distance-right (%right hub-distance)
                        :hub-distance-right (%right hub-distance)
                        :hub-spoke-hole-diameter (spoke-hole-diameter hub)
                        :hub-spoke-hole-offset (when (typep hub 'straight-pull-hub) (spoke-hole-offset hub))
                        :is-straight-pull-hub (when (typep hub 'straight-pull-hub) t)
                        :rim-label (component-name rim)
                        :rim-inner-diameter (inner-diameter rim)
                        :rim-thickness (thickness rim)
                        :rim-base-diameter (base-diameter rim)
                        :rim-diameter (* 2 rim-radius)
                        :rim-radius rim-radius
                        :rim-offset rim-offset
                        :is-asymmetric-rim (not (zerop rim-offset))
                        :spoke-label (component-name spoke)
                        :spoke-count (if (consp spoke-count) (+ (%left spoke-count) (%right spoke-count)) spoke-count)
                        :spoke-count-left (if (consp spoke-count) (%left spoke-count) (/ spoke-count 2))
                        :spoke-count-right (if (consp spoke-count) (%right spoke-count) (/ spoke-count 2))
                        :spoke-crossings-left (%left crossings)
                        :spoke-crossings-right (%right crossings)
                        :spoke-tension spoke-tension
                        :is-radial-spoked-left (%left radialp)
                        :is-radial-spoked-right (%right radialp)
                        :washer-label (when (typep washer 'washer) (component-name washer))
                        :washer-thickness (when washer washer-thickness)
                        :nipple-label (when (typep nipple 'nipple) (component-name nipple))
                        :nipple-length (when (typep nipple 'nipple) (nipple-length nipple))
                        :nipple-head-height (when (typep nipple 'nipple) (nipple-head-height nipple))
                        ;; Calculated.
                        :hub-effective-distance-left (%left hub-distance*)
                        :hub-effective-distance-right (%right hub-distance*)
                        :hub-sector-central-angle-left (%left hub-angle)
                        :hub-sector-central-angle-right (%right hub-angle)
                        :hub-step-size-left (%left hub-step)
                        :hub-step-size-right (%right hub-step)
                        :rim-sector-central-angle rim-angle
                        :rim-step-size rim-step
                        :rim-spoke-pair-step-size-left (%left rim-pair)
                        :rim-spoke-pair-step-size-right (%right rim-pair)
                        :tension-angle-left (%left alpha)
                        :tension-angle-right (%right alpha)
                        :tension-ratio-left (* 100 (%left tension-ratio))
                        :tension-ratio-right (* 100 (%right tension-ratio))
                        :spoke-distance-left (%left spoke-distance)
                        :spoke-distance-right (%right spoke-distance)
                        :spoke-length-left (%left spoke-length)
                        :spoke-length-right (%right spoke-length)
                        :spoke-tension-left (%left tension)
                        :spoke-tension-right (%right tension)
                        :spoke-elongation-left (%left spoke-elongation)
                        :spoke-elongation-right (%right spoke-elongation)
                        :minimum-spoke-length-left (%left minimum-length)
                        :minimum-spoke-length-right (%right minimum-length)
                        :maximum-spoke-length-left (%left maximum-length)
                        :maximum-spoke-length-right (%right maximum-length)
                        :valve-x (aref valve-hole 0)
                        :valve-y (aref valve-hole 1)
                        :valve-z (aref valve-hole 2)
                        :spokes (iter (for index :from 0 :below rim-holes)
                                      (collect (list :index index
                                                     :hub-x (aref hub-hole index 0)
                                                     :hub-y (aref hub-hole index 1)
                                                     :hub-z (aref hub-hole index 2)
                                                     :rim-x (aref rim-hole index 0)
                                                     :rim-y (aref rim-hole index 1)
                                                     :rim-z (aref rim-hole index 2)
                                                     :length (hypot3
                                                              (- (aref hub-hole index 0)
                                                                 (aref rim-hole index 0))
                                                              (- (aref hub-hole index 1)
                                                                 (aref rim-hole index 1))
                                                              (- (aref hub-hole index 2)
                                                                 (aref rim-hole index 2)))
                                                     :is-left (leftp (aref hub-hole index 3))
                                                     :is-right (rightp (aref hub-hole index 3))
                                                     :is-radial (radialp (aref hub-hole index 4))
                                                     :is-trailing (trailingp (aref hub-hole index 4))
                                                     :is-leading (leadingp (aref hub-hole index 4)))))
                        :data-format 1) ;sequence number
                  (let ((system (asdf:find-system :spoke-calculator)))
                    (list :software-title "Ralph's Spoke Calculator"
                          :software-description (asdf:system-description system)
                          :software-url "https://github.com/ralph-schleicher/spoke-calculator"
                          :software-name (asdf:component-name system)
                          :software-version (asdf:component-version system)))))))

(defun spoke-pattern (array start-index spoke-crossings rim-holes hub-holes hub-radius hub-offset side)
  (check-type start-index (integer 0))
  (check-type spoke-crossings (integer 0))
  (check-type rim-holes (integer (0)))
  (check-type hub-holes (integer (0)))
  (check-type hub-radius (real 0))
  (check-type hub-offset real)
  (when (null array)
    (setf array (make-array (list rim-holes 5) :initial-element 0)))
  (let* (;; Angle between two adjacent spoke holes on the rim (central
         ;; angle of a rim sector).
         (rim-hole-angle (/ 360 rim-holes))
         ;; Angle between two adjacent spoke holes on the hub (central
         ;; angle of a hub sector).
         (hub-hole-angle (/ 360 hub-holes))
         ;; Angle between the valve hole and the first spoke hole on
         ;; the rim.
         (rim-valve-angle (/ rim-hole-angle 2))
         ;; Angle between the valve hole and the first spoke hole on
         ;; the hub.
         (hub-valve-angle (if (zerop spoke-crossings)
                              rim-valve-angle
                            (/ (- hub-hole-angle rim-hole-angle) 2)))
         ;; Angle from the valve hole to the spoke hole indicated by
         ;; START-INDEX on the hub.  For triplet lacing, START-INDEX
         ;; must be zero for the side with spoke crossings.
         (start-angle (- 90 hub-valve-angle (* start-index rim-hole-angle))))
    (if (zerop spoke-crossings)
        ;; Radial spokes.  Either both sides radial or triplet lacing.
        (let ((rim-step (/ rim-holes hub-holes)))
          (check-type rim-step (integer 2 3))
          (iter (repeat hub-holes)
                (for index :from start-index :by rim-step)
                (for angle :from start-angle :by (- hub-hole-angle))
                (for angle* = (float angle pi))
                (setf (aref array index 0) (* hub-radius (cosd angle*))
                      (aref array index 1) (* hub-radius (sind angle*))
                      (aref array index 2) hub-offset
                      (aref array index 3) side
                      (aref array index 4) +radial+)))
      ;; Crossed spokes.
      (let* ((hub-holes* (/ hub-holes 2))
             (hub-hole-angle* (* hub-hole-angle 2))
             (rim-step (/ rim-holes hub-holes*))
             (index-step (- rim-step 2)))
        (check-type rim-step (integer 3 4))
        ;; Trailing spokes.
        (iter (repeat hub-holes*)
              (for index :from start-index :by rim-step)
              (for angle :from (- start-angle (* spoke-crossings hub-hole-angle))
                         :by (- hub-hole-angle*))
              (for angle* = (float angle pi))
              (setf (aref array index 0) (* hub-radius (cosd angle*))
                    (aref array index 1) (* hub-radius (sind angle*))
                    (aref array index 2) hub-offset
                    (aref array index 3) side
                    (aref array index 4) +trailing+))
        ;; Leading spokes.
        (iter (repeat hub-holes*)
              (for index :from (+ start-index index-step) :by rim-step)
              (for angle :from (+ start-angle (* (1- spoke-crossings) hub-hole-angle))
                         :by (- hub-hole-angle*))
              (for angle* = (float angle pi))
              (setf (aref array index 0) (* hub-radius (cosd angle*))
                    (aref array index 1) (* hub-radius (sind angle*))
                    (aref array index 2) hub-offset
                    (aref array index 3) side
                    (aref array index 4) +leading+))))
    array))

(defun spoke-elongation (spoke length tension)
  "Calculate the elongation of a spoke under tension load.

First argument SPOKE is a ‘spoke’ object.
Second argument LENGTH is the length of the spoke in millimeter.
Third argument TENSION is the spoke tension in kilogram.  Value
 has to be a non-negative number.

The cross-sectional area and the length of the spoke's middle section
and the material of the spoke must be defined.  See the ‘spoke-area’,
‘spoke-middle-section’, and ‘spoke-material’ methods of the ‘spoke’
class.

Primary value is the elongation of the spoke in millimeter.  Secondary
value is true if the corresponding stress is less than or equal to the
spoke's strength."
  ;; ε = Δℓ/ℓ₀ = 1/E ⋅ F/A
  (let ((force (* tension gn))
        (area (spoke-area spoke))
        (strength (spoke-strength spoke))
        (elastic-modulus (let ((material (spoke-material spoke)))
                           (etypecase material
                             (material
                              (elastic-modulus material))
                             (string
                              (when-let ((material (find-material material)))
                                (elastic-modulus material)))
                             (null))))
        ;; Length of the spoke's middle section.
        (middle-section (let ((value (spoke-middle-section spoke)))
                          (cond ((functionp value)
                                 (funcall value spoke length))
                                ((plusp value)
                                 ;; Absolute length.
                                 value)
                                (;; Relative length.
                                 (+ length value))))))
    (if (and area (plusp area) elastic-modulus (plusp elastic-modulus))
        (let* ((stress (/ force area))
               (strain (/ stress elastic-modulus)))
          (values (* strain (max 0 middle-section))
                  (when (and strength (plusp strength)) (<= stress strength))))
      0)))

;;; calculate.lisp ends here
