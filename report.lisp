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
  "The default directory for saving a report.
Default is the user's home directory.")

(defvar *output-name* (make-pathname :name "spoke-calculator")
  "The default file name of a report.
Default is ‘spoke-calculator’.")

;;;; Text

(defvar *text-type* (make-pathname :type "txt")
  "The file type of a text report.  Default is ‘txt’.")

(defun text-report (data)
  "Generate a text report."
  (let ((*read-default-float-format* 'double-float)
        (*print-case* :downcase))
    (fresh-line)
    (format t "~@[~A~%~]" (getf data :title))
    (format t "~@[Hub ~A~%~]" (getf data :hub-label))
    (format t "Hub Diameter: ~S~%" (cons (getf data :hub-diameter-left) (getf data :hub-diameter-right)))
    (format t "Hub Radius: ~S~%" (cons (getf data :hub-radius-left) (getf data :hub-radius-right)))
    (format t "Hub Distance: ~S~%" (cons (getf data :hub-distance-left) (getf data :hub-distance-right)))
    (format t "Hub Spoke Hole Diameter: ~S~%" (getf data :hub-spoke-hole-diameter))
    (format t "Hub Spoke Hole Offset: ~S~%" (getf data :hub-spoke-hole-offset))
    (format t "~@[Rim ~A~%~]" (getf data :rim-label))
    (format t "Rim Inner Diameter: ~S~%" (getf data :rim-inner-diameter))
    (format t "Rim Thickness: ~S~%" (getf data :rim-thickness))
    (format t "Rim Base Diameter: ~S~%" (getf data :rim-base-diameter))
    (format t "Washer Thickness: ~S~%" (getf data :washer-thickness))
    (format t "Rim Diameter: ~S~%" (getf data :rim-diameter))
    (format t "Rim Radius: ~S~%" (getf data :rim-radius))
    (format t "Rim Offset: ~S~%" (getf data :rim-offset))
    (format t "~@[Spoke ~A~%~]" (getf data :spoke-label))
    (format t "Number of Spokes: ~S~%" (getf data :spoke-count))
    (format t "Spoke Crossings: ~S~%" (cons (getf data :spoke-crossings-left) (getf data :spoke-crossings-right)))
    (format t "Hub Holes: ~S~%" (cons (getf data :spoke-count-left) (getf data :spoke-count-right)))
    (format t "Hub Sector Central Angle: ~S~%" (cons (getf data :hub-sector-central-angle-left) (getf data :hub-sector-central-angle-right)))
    (format t "Hub Step Size: ~S~%" (cons (getf data :hub-step-size-left) (getf data :hub-step-size-right)))
    (format t "Rim Holes: ~S~%" (getf data :spoke-count))
    (format t "Rim Sector Central Angle: ~S~%" (getf data :rim-sector-central-angle))
    (format t "Rim Step Size: ~S~%" (getf data :rim-step-size))
    (format t "Rim Spoke Pair Step Size: ~S~%" (cons (getf data :rim-spoke-pair-step-size-left) (getf data :rim-spoke-pair-step-size-right)))
    (format t "Tension Angle: ~S~%" (cons (getf data :tension-angle-left) (getf data :tension-angle-right)))
    (format t "Tension Ratio: ~S~%" (cons (getf data :tension-ratio-left) (getf data :tension-ratio-right)))
    (format t "Geometric Spoke Length: ~S~%" (cons (getf data :spoke-distance-left) (getf data :spoke-distance-right)))
    (format t "Actual Spoke Length: ~S~%" (cons (getf data :spoke-length-left) (getf data :spoke-length-right)))
    (format t "Spoke Tension: ~S~%" (cons (getf data :spoke-tension-left) (getf data :spoke-tension-right)))
    (format t "Spoke Elongation: ~S~%" (cons (getf data :spoke-elongation-left) (getf data :spoke-elongation-right)))
    (format t "Minimum Spoke Length: ~S~%" (cons (getf data :minimum-spoke-length-left) (getf data :minimum-spoke-length-right)))
    (format t "Maximum Spoke Length: ~S~%" (cons (getf data :maximum-spoke-length-left) (getf data :maximum-spoke-length-right)))
    ()))

;;;; HTML

(defvar *html-type* (make-pathname :type "html")
  "The file type of a HTML report.  Default is ‘html’.")

(defvar *html-template* (merge-pathnames
                         #P"report.html.in"
                         (load-time-value
                          (asdf:system-source-directory :spoke-calculator)))
  "The HTML template file.

The template file is coded up by the user.  The ‘report’ function uses
Edi Weitz's HTML-TEMPLATE library for filling in the documentation
strings.  Below is the list of template tags together with their
meaning.")

(defun html-esc (object)
  (etypecase object
    ((or boolean integer)
     object)
    ((or float rational)
     (coerce object 'single-float))
    (string
     (cl-who:escape-string-minimal object))
    (list
     (if (consp (first object))
         (mapcar #'html-esc object)
       (iter (for (key value) :on object :by #'cddr)
             (nconcing (list key (html-esc value))))))))

(defun html-report (data)
  "Generate a HTML report."
  (let ((html-template:*warn-on-creation* nil)
        (html-template:*default-template-output* *standard-output*)
        (html-template:*string-modifier* #'identity)
        (html-template:*ignore-empty-lines* t))
    (html-template:fill-and-print-template *html-template* (html-esc data))))

;;;; Common Interface

(defun report (data &key (output *standard-output*) (format :text) title)
  "Generate a report.

First argument DATA is a result data set of the ‘calculate’ function.
 Default is the result data set stored in ‘*data*’.

Keyword argument OUTPUT specifies the destination.  Value is either
 a stream, a pathname, a string, or ‘nil’.  Default is the value of
 the ‘*standard-output*’ special variable.  If OUTPUT is a pathname,
 the special variables ‘*output-directory*’, ‘*output-name*’,
 ‘*text-type*’, and ‘*html-type*’ have an effect.  If OUTPUT is ‘t’,
 use the special variables ‘*output-name*’ or ‘*output-directory*’.
 If OUTPUT is ‘nil’, the return value is a string.
Keyword argument FORMAT specifies the file format of the report.
 Value is either ‘:text’ or ‘:html’.  Default is ‘:text’.
Keyword argument TITLE defines the title for the report (a string).

Return value is ‘nil’ or a string."
  (check-type format (member :text :html))
  (check-type title (or null string))
  (setf data (nconc (list :title title)
                    (or data *data*)))
  (when (eq output t)
    (setf output (or *output-name*
                     *output-directory*
                     (user-homedir-pathname)))
    (when (stringp output)
      (setf output (uiop:parse-native-namestring output))))
  (let ((*read-default-float-format* 'single-float))
    (flet ((ensure-dir (destination)
             (ignore-errors
              (ensure-directories-exist
               (make-pathname
                :directory (pathname-directory (pathname destination))))))
           (generate ()
             (ecase format
               (:text
                (text-report data))
               (:html
                (html-report data)))
             ;; Return value.
             nil))
      (etypecase output
        (stream
         (ensure-dir output)
         (let ((*standard-output* output))
           (generate)))
        (pathname
         (unless (or (pathname-directory output) (null *output-directory*))
           (setf output (uiop:merge-pathnames* output *output-directory*)))
         (unless (or (pathname-name output) (null *output-name*))
           (setf output (uiop:merge-pathnames* output *output-name*)))
         (let ((type (ecase format
                       (:text
                        *text-type*)
                       (:html
                        *html-type*))))
           (unless (or (pathname-type output) (null type))
             (setf output (uiop:merge-pathnames* output type))))
         (ensure-dir output)
         (with-open-file (stream output
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (let ((*standard-output* stream))
             (generate))))
        (string
         (with-output-to-string (stream output)
           (let ((*standard-output* stream))
             (generate))))
        (null
         (with-output-to-string (stream)
           (let ((*standard-output* stream))
             (generate))))))))

;;; report.lisp ends here
