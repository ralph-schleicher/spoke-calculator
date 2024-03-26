;;; materials.lisp --- material properties

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

;;; Code:

(in-package :spoke-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

(defvar *elastic-modulus* 180E+3
  "Modulus of elasticity in megapascal.

The value of this special variable is used if no spoke material
is defined.  Most spokes are made of stainless steel according
to ISO 6931-1, i.e. spring wire in the cold drawn condition with
a modulus of elasticity of 180 MPa.")

(defclass material ()
  ((name
    :documentation "The material name."
    :accessor material-name
    :initarg :name
    :initform (error "Missing material name.")
    :type string)
   (elastic-modulus
    :documentation "Modulus of elasticity in megapascal."
    :accessor elastic-modulus
    :initarg :elastic-modulus
    :initform nil
    :type (or null (real 0)))))

(defmethod material-name ((object null))
  (declare (ignore object))
  "N.N.")

(defmethod elastic-modulus ((object null))
  (declare (ignore object))
  *elastic-modulus*)

(defvar material-table ()
  "The material database.")

(defun find-material (name)
  (check-type name string)
  (cdr (assoc name material-table :test #'string-equal)))

(defun define-material (name &rest options &key elastic-modulus)
  (check-type name string)
  (check-type elastic-modulus (or null (real 0)))
  (let ((material (find-material name)))
    (if (null material)
        (push (cons name (apply #'make-instance 'material :name name options)) material-table)
      (setf (elastic-modulus material) elastic-modulus)))
  (values))

(define-material "X10CrNi18-8 cold drawn"
  :elastic-modulus 180E+3)

(define-material "X10CrNi18-8 stress relieved"
  :elastic-modulus 185E+3)

(define-material "AISI 302"
  :elastic-modulus 180E+3)

;;; materials.lisp ends here
