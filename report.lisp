;;; report.lisp --- generate a report

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

(defvar *output-directory* (user-homedir-pathname)
  "The default directory for saving a report.")

(defvar *output-name* (make-pathname :name "spoke-calculator")
  "The default file name of a report.")

;;;; Text

(defun text-report (data)
  (let ((*read-default-float-format* 'double-float))
    (terpri)
    (format t "Hub~@[ ~A~]~%" (getf data :hub-label))
    (format t "Hub Diameter: ~S~%" (cons (getf data :hub-diameter-left) (getf data :hub-diameter-right)))
    (format t "Hub Radius: ~S~%" (cons (getf data :hub-radius-left) (getf data :hub-radius-right)))
    (format t "Hub Distance: ~S~%" (cons (getf data :hub-distance-left) (getf data :hub-distance-right)))
    (format t "Installation Width: ~S~%" (getf data :hub-installation-width))
    (format t "Flange Distance: ~S~%" (cons (getf data :hub-flange-distance-left) (getf data :hub-flange-distance-right)))
    (format t "Rim ~A~%" (getf data :rim-label))
    (format t "Rim Base Diameter: ~S~%" (getf data :rim-base-diameter))
    (format t "Washer Thickness: ~S~%" (getf data :washer-thickness))
    (format t "Effective Rim Diameter: ~S~%" (getf data :rim-diameter))
    (format t "Rim Radius: ~S~%" (getf data :rim-radius))
    (format t "Rim Offset: ~S~%" (getf data :rim-offset))
    (format t "Number of Spokes: ~S~%" (getf data :spoke-count))
    (format t "Spoke Crossings: ~S~%" (getf data :spoke-crossings))
    (format t "Hub Holes: ~S~%" (cons (getf data :spoke-count-left) (getf data :spoke-count-right)))
    (format t "Hub Sector Central Angle: ~S~%" (cons (getf data :hub-sector-central-angle-left) (getf data :hub-sector-central-angle-right)))
    (format t "Hub Step: ~S~%" (cons (getf data :hub-step-size-left) (getf data :hub-step-size-right)))
    (format t "Rim Holes: ~S~%" (getf data :spoke-count))
    (format t "Rim Sector Central Angle: ~S~%" (getf data :rim-sector-central-angle))
    (format t "Rim Step: ~S~%" (getf data :rim-step-size))
    (format t "Rim Sectors: ~S~%" (cons (getf data :rim-spoke-pair-step-size-left) (getf data :rim-spoke-pair-step-size-right)))
    (format t "Tension Angle: ~S~%" (cons (getf data :tension-angle-left) (getf data :tension-angle-right)))
    (format t "Tension Ratio: ~S~%" (cons (getf data :tension-ratio-left) (getf data :tension-ratio-right)))
    (format t "Geometric Spoke Length: ~S~%" (cons (getf data :spoke-distance-left) (getf data :spoke-distance-right)))
    (format t "Actual Spoke Length: ~S~%" (cons (getf data :spoke-length-left) (getf data :spoke-length-right)))
    (format t "Spoke Elongation: ~S, max. spoke tension ~S kg~%" (cons (getf data :spoke-elongation-left) (getf data :spoke-elongation-right)) (getf data :spoke-tension))
    (format t "Minimum Spoke Length: ~S~%" (cons (getf data :minimum-spoke-length-left) (getf data :minimum-spoke-length-right)))
    (format t "Maximum Spoke Length: ~S~%" (cons (getf data :maximum-spoke-length-left) (getf data :maximum-spoke-length-right)))
    ()))

;;;; HTML

(defvar *html-template* (merge-pathnames
                         #P"report.html.in"
                         (load-time-value
                          (asdf:system-source-directory :spoke-calculator)))
  "The HTML template file.

The template file is coded up by the user.  The ‘report’ function uses
Edi Weitz's HTML-TEMPLATE library for filling in the documentation
strings.  Below is the list of template tags together with their
meaning.")

(defun esc (object)
  (etypecase object
    ((or boolean integer)
     object)
    ((or float rational)
     (coerce object 'single-float))
    (string
     (cl-who:escape-string-minimal
      object))
    (list
     (if (consp (first object))
         (mapcar #'esc object)
       (iter (for (key value) :on object :by #'cddr)
             (nconcing (list key (esc value))))))))

(defun html-report (data)
  "Generate HTML."
  (let ((html-template:*default-template-output* *standard-output*)
        (html-template:*string-modifier* #'identity)
        (html-template:*ignore-empty-lines* t))
    (html-template:fill-and-print-template *html-template* (esc data))))

(defun report (data &key (output *output-directory*) (format :html))
  "Generate a report.

First argument DATA is a result data set of the ‘calculate’ function.
Keyword argument OUTPUT specifies the destination.  Value is either
 a stream, a pathname, or a string.  The special value ‘t’ means
 ‘*standard-output*’ and ‘nil’ returns a string.  If OUTPUT is a
 pathname, the special variables ‘*output-directory*’ and
 ‘*output-name*’ have an effect.
Keyword argument FORMAT specifies the file format of the report.
 Value is either ‘:text’ or ‘:html’."
  (let ((*read-default-float-format* 'single-float))
    (labels ((ensure-dir (destination)
               (ignore-errors
                (ensure-directories-exist
                 (make-pathname
                  :directory (pathname-directory (pathname destination))))))
             (generate ()
               (ecase format
                 (:text
                  (text-report data))
                 (:html
                  (html-report data)))))
      (etypecase output
        ((member t)
         (ensure-dir *standard-output*)
         (generate))
        (stream
         (ensure-dir output)
         (let ((*standard-output* output))
           (generate)))
        (pathname
         (unless (pathname-directory output)
           (setf output (uiop:merge-pathnames*
                         output *output-directory*)))
         (unless (pathname-name output)
           (setf output (uiop:merge-pathnames*
                         output *output-name*)))
         (unless (or (pathname-type output) (eq format :text))
           (setf output (uiop:merge-pathnames*
                         output (make-pathname :type (string-downcase format)))))
         (ensure-dir output)
         (with-open-file (stream output
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (let ((*standard-output* stream))
             (generate))))
        (null
         (with-output-to-string (stream)
           (let ((*standard-output* stream))
             (generate))))
        (string
         (with-output-to-string (stream output)
           (let ((*standard-output* stream))
             (generate))))))))

;;; report.lisp ends here
