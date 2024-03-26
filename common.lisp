;;; common.lisp --- common definitions

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

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(defconst gn 9.80665
  "Standard acceleration of gravity.")

(defun hypot (x y)
  "Return the distance between a point and the origin
in a two-dimensional Cartesian coordinate system.

Arguments X and Y have to be real numbers."
  (declare (type real x y))
  (abs (complex x y)))

(defun hypot3 (x y z)
  "Return the distance between a point and the origin
in a three-dimensional Cartesian coordinate system.

Arguments X, Y, and Z have to be real numbers."
  (declare (type real x y z))
  (setf x (abs x)
        y (abs y)
        z (abs z))
  ;; Scale by largest element.
  (let ((s (max x y z)))
    (when (/= s 0)
      (setf x (/ x s)
            y (/ y s)
            z (/ z s)))
    (* s (sqrt (+ (* x x) (* y y) (* z z))))))

(defun radian-from-degree (deg)
  "Convert a plane angle from degree to radian.

Argument DEG is the angle given in degree.

Value is the corresponding angle given in radian."
  (declare (type real deg))
  (* (/ deg 180) pi))

(defun degree-from-radian (rad)
  "Convert a plane angle from radian to degree.

Argument RAD is the angle given in radian.

Value is the corresponding angle given in degree."
  (declare (type real rad))
  (* (/ rad pi) 180))

(defun sind (number)
  "Return the sine of NUMBER given in degree angle."
  (sin (radian-from-degree number)))

(defun cosd (number)
  "Return the cosine of NUMBER given in degree angle."
  (cos (radian-from-degree number)))

(defun area-of-circle (diameter)
  "Calculate the area of a circle."
  (check-type diameter (real 0))
  (* pi (expt (/ diameter 2) 2)))

(defun area-of-ellipse (width height)
  "Calculate the area of an ellipse."
  (check-type width (real 0))
  (check-type height (real 0))
  (* pi (/ width 2) (/ height 2)))

(defun area-of-rectangle (width height &optional radius)
  "Calculate the area of a rounded rectangle."
  (check-type width (real 0))
  (check-type height (real 0))
  (when (< width height)
    (rotatef width height))
  (if (null radius)
      (setf radius (/ height 2))
    (let ((type-spec `(real 0 ,(/ height 2))))
      (unless (typep radius type-spec)
        (error 'type-error :datum radius :expected-type type-spec))))
  (- (* width height) (* (- 4 pi) (expt radius 2))))

(defsubst %left (object)
  (car object))

(defsubst %right (object)
  (cdr object))

(defun left (object)
  "Return the left-hand side object."
  (if (consp object) (car object) object))

(defun right (object)
  "Return the right-hand side object."
  (if (consp object) (cdr object) object))

(defun %round (function number divisor)
  (check-type number float)
  (check-type divisor rational)
  (float (* (funcall function number divisor) divisor) number))

(defsubst round-up (number &optional (divisor 1))
  "Round towards positive infinity."
  (%round #'ceiling number divisor))

(defsubst round-down (number &optional (divisor 1))
  "Round towards negative infinity."
  (%round #'floor number divisor))

(defsubst round-near (number &optional (divisor 1))
  "Round to the nearest multiple."
  (%round #'round number divisor))

;;; common.lisp ends here
