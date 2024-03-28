;;; packages.lisp --- package definitions

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

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.spoke-calculator
  (:nicknames :spoke-calculator)
  (:use :common-lisp
        :iterate)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export
   ;; common.lisp
   #:gn
   #:hypot
   #:hypot3
   #:radian-from-degree
   #:degree-from-radian
   #:sind
   #:cosd
   #:area-of-circle
   #:area-of-ellipse
   #:area-of-rectangle
   #:left
   #:right
   #:round-up
   #:round-down
   #:round-near
   ;; materials.lisp
   #:*elastic-modulus*
   #:define-material
   #:find-material
   #:material-name
   #:elastic-modulus
   ;; components.lisp
   #:pitch-circle-diameter
   #:pitch-circle-distance
   #:hub #:straight-pull-hub
   #:spoke-hole-diameter
   #:spoke-hole-offset
   #:wheel-axle
   #:rim
   #:base-diameter
   #:first-spoke-hole
   #:component
   #:component-name
   #:axle #:thru-axle
   #:axle-diameter
   #:axle-length
   #:axle-thread
   #:spoke
   #:spoke-diameter
   #:spoke-area
   #:spoke-strength
   #:spoke-material
   #:spoke-middle-section
   #:spoke-thread
   #:nipple
   #:nipple-length
   #:nipple-head-height
   #:washer
   #:washer-thickness
   #:make-hub-from-measurements
   #:make-rim-from-measurements
   ;; calculate.lisp
   #:*default-spoke*
   #:*default-nipple*
   #:*data*
   #:calculate
   #:spoke-elongation
   ;; report.lisp
   #:*output-directory*
   #:*output-name*
   #:*text-type*
   #:*html-type*
   #:*html-template*
   #:report
   ;; data/dtswiss.lisp
   #:dtswiss-revolite
   #:dtswiss-aerolite
   #:dtswiss-aero-comp
   #:dtswiss-revolution
   #:dtswiss-competition-race
   #:dtswiss-competition
   #:dtswiss-alpine-iii
   #:dtswiss-alpine
   #:dtswiss-standard
   #:dtswiss-standard-14
   #:dtswiss-standard-14.5
   #:dtswiss-standard-16
   #:dtswiss-squorx
   #:dtswiss-hidden
   #:dtswiss-pro-head
   #:dtswiss-pro-head-14
   #:dtswiss-phr
   ;; data/sapim.lisp
   #:sapim-cx-ray
   #:sapim-cx-sprint
   #:sapim-laser
   #:sapim-d-light
   #:sapim-race
   #:sapim-strong
   #:sapim-polyax
   #:sapim-flat-head
   #:sapim-round-head
   #:sapim-double-square
   #:sapim-up-side-down
   #:sapim-hm
   #:sapim-mg
   #:sapim-ms
   #:sapim-flat
   #:sapim-oval-small
   #:sapim-oval-large
   ;; data/standard.lisp
   #:quick-release-5x100
   #:quick-release-5x130
   #:quick-release-5x135
   #:thru-axle-20x100
   #:thru-axle-15x100
   #:thru-axle-12x100
   #:thru-axle-20x110
   #:thru-axle-15x110
   #:thru-axle-12x142
   #:thru-axle-12x148
   #:thru-axle-12x157
   ;; data/syntace.lisp
   #:syntace-x12-100
   #:syntace-x12-142
   #:syntace-x12-148
   #:syntace-x12-157)
  (:documentation
   "A spoke length calculator for bicycle wheels.

To use it, you need the basic dimensions of the hub and the rim and an
idea about the spoke pattern.

     (in-package :spoke-calculator-user)

     (calculate :hub (make-instance
                      'hub :pitch-circle-diameter (cons 56.0 54.0)
                           :pitch-circle-distance (cons 32.0 21.0)
                           :spoke-hole-diameter 2.5)
                :rim (make-instance
                      'rim :base-diameter 536.0)
                :spoke sapim-race
                :spoke-count 32
                :spoke-crossings 3)

     ;; Create a report from the last result data set.
     (report *data*)

The spoke calculator can handle the regular spoke patterns with no,
one, two, three, or four spoke crossings.  The left-hand side and
right-hand side of the wheel can have a different spoke pattern,
e.g. for triplet lacing."))

(defpackage #:de.ralph-schleicher.spoke-calculator-user
  (:nicknames :spoke-calculator-user)
  (:use :common-lisp
        :spoke-calculator)
  (:documentation
   "Workbench for the spoke length calculator."))

;;; packages.lisp ends here
