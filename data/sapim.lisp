;;; sapim.lisp --- Sapim spokes and nipples

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

;; https://www.sapim.be/

;;; Code:

(in-package :spoke-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

(defparameter sapim-cx-ray
  (make-instance 'spoke
                 :name "Sapim CX-Ray"
                 :diameter 2.0
                 :area (area-of-ellipse 2.2 0.9)
                 :strength 1600.0
                 :material "AISI 302"
                 :middle-section -40.0
                 :thread "14G"))

(defparameter sapim-cx-sprint
  (make-instance 'spoke
                 :name "Sapim CX-Sprint"
                 :diameter 2.0
                 :area (area-of-ellipse 2.25 1.25)
                 :strength 1430.0
                 :material "AISI 302"
                 :middle-section -40.0
                 :thread "14G"))

(defparameter sapim-laser
  (make-instance 'spoke
                 :name "Sapim Laser"
                 :diameter 2.0
                 :area (area-of-circle 1.5)
                 :strength 1500.0
                 :material "AISI 302"
                 :middle-section -40.0
                 :thread "14G"))

;; The Sapim D-Light is a safe choice since the thread runs out into
;; the middle section, i.e. the thread doesn't block if the spoke is
;; too long.
(defparameter sapim-d-light
  (make-instance 'spoke
                 :name "Sapim D-Light"
                 :diameter 2.0
                 :area (area-of-circle 1.65)
                 :strength 1370.0
                 :material "AISI 302"
                 :middle-section -25.0
                 :thread "14G"))

(defparameter sapim-race
  (make-instance 'spoke
                 :name "Sapim Race"
                 :diameter 2.0
                 :area (area-of-circle 1.8)
                 :strength 1300.0
                 :material "AISI 302"
                 :middle-section -40.0
                 :thread "14G"))

(defparameter sapim-strong
  (make-instance 'spoke
                 :name "Sapim Strong"
                 :diameter 2.3
                 :area (area-of-circle 2.0)
                 :strength 1250.0
                 :material "AISI 302"
                 :middle-section -30.0
                 :thread "14G"))

;;;; Nipples

(defparameter sapim-polyax
  (make-instance 'nipple
                 :name "Sapim Polyax"
                 :length 12.0
                 :head-height 3.0))

(defparameter sapim-flat-head
  (make-instance 'nipple
                 :name "Sapim Flat Head"
                 :length 12.0
                 :head-height 3.0))

(defparameter sapim-round-head
  (make-instance 'nipple
                 :name "Sapim Round Head"
                 :length 12.0
                 :head-height 3.0))

(defparameter sapim-double-square
  (make-instance 'nipple
                 :name "Sapim Double Square"
                 :length 15.0
                 :head-height 6.0))

(defparameter sapim-up-side-down
  (make-instance 'nipple
                 :name "Sapim Up Side Down"
                 :length 12.0
                 :head-height 12.0))

;;;; Washers

(defparameter sapim-hm
  (make-instance 'washer
                 :name "Sapim Nipple Washer HM"
                 ;; 7.2 mm × 4.5 mm, conical, for Polyax nipples
                 :thickness 0.5))

(defparameter sapim-mg
  (make-instance 'washer
                 :name "Sapim Nipple Washer MG"
                 ;; 7.5 mm × 5 mm × 2 mm, symmetric, for Polyax nipples
                 :thickness 1.8))

(defparameter sapim-ms
  (make-instance 'washer
                 :name "Sapim Nipple Washer MS"
                 ;; 7.5 mm × 5 mm × 1.5 mm
                 :thickness 1.5))

(defparameter sapim-flat
  (make-instance 'washer
                 :name "Sapim Nipple Washer Flat"
                 ;; 9 mm × 5 mm
                 :thickness 0.5))

(defparameter sapim-oval-small
  (make-instance 'washer
                 :name "Sapim Nipple Washer Oval"
                 ;; 9.5/14 mm × 4.5 mm
                 :thickness 0.6))

(defparameter sapim-oval-large
  (make-instance 'washer
                 :name "Sapim Nipple Washer Oval"
                 ;; 11/19 mm × 5.5 mm
                 :thickness 0.6))

;;; sapim.lisp ends here
