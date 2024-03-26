;;; dtswiss.lisp --- DT Swiss components

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

;; https://www.dtswiss.com/

;;; Code:

(in-package :spoke-calculator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

;;;; Spokes

(defparameter dtswiss-revolite
  (make-instance 'spoke
                 :name "DT Swiss Revolite"
                 :diameter 2.0
                 :area (area-of-circle 1.57) ;2.3×1.3
                 :thread "14G"))

(defparameter dtswiss-aerolite
  (make-instance 'spoke
                 :name "DT Swiss Aerolite"
                 :diameter 2.0
                 :area (area-of-circle 1.5) ;2.3×0.9
                 :thread "14G"))

(defparameter dtswiss-aero-comp
  (make-instance 'spoke
                 :name "DT Swiss Aero Comp"
                 :diameter 2.0
                 :area (area-of-circle 1.8) ;2.3×1.2
                 :thread "14G"))

(defparameter dtswiss-revolution
  (make-instance 'spoke
                 :name "DT Swiss Revolution"
                 :diameter 2.0
                 :area (area-of-circle 1.5)
                 :thread "14G"))

(defparameter dtswiss-competition-race
  (make-instance 'spoke
                 :name "DT Swiss Competition Race"
                 :diameter 2.0
                 :area (area-of-circle 1.6)
                 :thread "14G"))

(defparameter dtswiss-competition
  (make-instance 'spoke
                 :name "DT Swiss Competition"
                 :diameter 1.8
                 :area (area-of-circle 1.6)
                 :thread "15G"))

(defparameter dtswiss-alpine-iii
  (make-instance 'spoke
                 :name "DT Swiss Alpine III"
                 :diameter 2.0
                 :area (area-of-circle 1.85)
                 :thread "14G"))

(defparameter dtswiss-alpine
  (make-instance 'spoke
                 :name "DT Swiss Alpine"
                 :diameter 2.0
                 :area (area-of-circle 2.34)
                 :thread "14G"))

;;;; Nipples

(defparameter dtswiss-standard
  (make-instance 'nipple
                 :name "DT Swiss Standard"
                 :length 12.0
                 :head-height 3.0))

(defparameter dtswiss-standard-14
  (make-instance 'nipple
                 :name "DT Swiss Standard"
                 :length 14.0
                 :head-height 3.0))

(defparameter dtswiss-standard-14.5
  (make-instance 'nipple
                 :name "DT Swiss Standard"
                 :length 14.5
                 :head-height 3.0))

(defparameter dtswiss-standard-16
  (make-instance 'nipple
                 :name "DT Swiss Standard"
                 :length 16.0
                 :head-height 3.0))

(defparameter dtswiss-squorx
  (make-instance 'nipple
                 :name "DT Swiss Squorx"
                 :length 15.0
                 :head-height 6.0))

(defparameter dtswiss-hidden
  (make-instance 'nipple
                 :name "DT Swiss Hidden"
                 :length 12.0
                 :head-height 12.0))

(defparameter dtswiss-pro-head
  (make-instance 'nipple
                 :name "DT Swiss Pro Head"
                 :length 12.0
                 :head-height 3.0))

(defparameter dtswiss-pro-head-14
  (make-instance 'nipple
                 :name "DT Swiss Pro Head"
                 :length 14.0
                 :head-height 3.0))

;;;; Washers

(defparameter dtswiss-phr
  (make-instance 'washer
                 :name "DT Swiss PHR"
                 :thickness 0.9))

;;; dtswiss.lisp ends here
