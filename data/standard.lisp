;;; standard.lisp --- standard components

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

;;;; Wheel Axles

(defparameter quick-release-5x100
  (make-instance 'axle
                 :name "Quick Release, 5 mm × 100 mm"
                 :diameter 5
                 :length 100))

(defparameter quick-release-5x130
  (make-instance 'axle
                 :name "Quick Release, 5 mm × 130 mm"
                 :diameter 5
                 :length 130))

(defparameter quick-release-5x135
  (make-instance 'axle
                 :name "Quick Release, 5 mm × 135 mm"
                 :diameter 5
                 :length 135))

(defparameter thru-axle-20x100
  (make-instance 'axle
                 :name "Thru Axle, 20 mm × 100 mm"
                 :diameter 20
                 :length 100))

(defparameter thru-axle-15x100
  (make-instance 'axle
                 :name "Thru Axle, 15 mm × 100 mm"
                 :diameter 15
                 :length 100))

(defparameter thru-axle-12x100
  (make-instance 'axle
                 :name "Thru Axle, 12 mm × 100 mm"
                 :diameter 12
                 :length 100))

(defparameter thru-axle-20x110
  (make-instance 'axle
                 :name "Thru Axle, 20 mm × 110 mm"
                 :diameter 20
                 :length 110))

(defparameter thru-axle-15x110
  (make-instance 'axle
                 :name "Thru Axle, 15 mm × 110 mm"
                 :diameter 15
                 :length 110))

(defparameter thru-axle-12x142
  (make-instance 'axle
                 :name "Thru Axle, 12 mm × 142 mm"
                 :diameter 12
                 :length 142))

(defparameter thru-axle-12x148
  (make-instance 'axle
                 :name "Thru Axle, 12 mm × 148 mm"
                 :diameter 12
                 :length 148))

(defparameter thru-axle-12x157
  (make-instance 'axle
                 :name "Thru Axle, 12 mm × 157 mm"
                 :diameter 12
                 :length 157))

;;; standard.lisp ends here
