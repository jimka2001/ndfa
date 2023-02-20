;; Copyright (c) 2016,18 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :ndfa)

(defgeneric ndfa-to-dot (object stream &rest others &key state-legend transition-legend equal-transition-labels transition-abrevs transition-label-cb view prefix title state< report-labels))

(defun transition-label-cb (label name)
  (declare (ignore label name))
  nil)

(defun ndfa-state< (state1 state2)
  (cmp-objects (state-label state1) (state-label state2)))

(defmethod ndfa-to-dot ((ndfa state-machine) stream &key transition-abrevs
                                                      (transition-label-cb #'transition-label-cb)
                                                      (view nil) prefix title
                                                      (state< #'ndfa-state<)
                                                      (report-labels nil)
                        &allow-other-keys )
  "Generate a dot file (for use by graphviz).  The dot file illustrates the states
 and transitions of the NDFA state machine.  The dot file is written to STREAM
 which may be any valid first argument of FORMAT, but is usually t or a stream object.
 TRANSITION-ABREVS (a car/cadr alist) mapping type specifiers to symbolic labels.
 If such a type specifier is found in the ndfa, and TRANSITION-LEGEND is true,
 then the name indicated in TRANSITION-ABREVS is used, otherwise a new symbolic name
 is generated.   This feature allows you to create multiple NDFA graphs using the
 same state transition lablels.
 If STATE-LEGEND is nil, then state labels in the graphical output will correspond to
 the (state-legend ...) of the state."
  (declare (ignore view)
	   (type (or null string) title)
	   (type (or (member t nil) stream))
           (type (function (t t) t) transition-label-cb
                 state<)
	   (ignore prefix))
  (relabel-states ndfa)
  (flet ((stringify (data)
	   (cond ((null data)
		  nil)
		 ((listp data)
		  (with-output-to-string (str)
		    (write data :case :downcase :stream str)))
		 (t
		  (with-output-to-string (str)
		    (write data :case :downcase :stream str)))))
	 (new-transition-name (transition)
	   (let* ((transition-label (transition-label transition))
                  (hit (assoc transition-label transition-abrevs :test (transition-label-equal ndfa)))
                  (new-name (cond (hit
                                   (cadr hit))
                                  (t
                                   (let ((proposed-name "T1")
                                         (transition-index 1))
                                     (while (rassoc proposed-name transition-abrevs :test #'string= :key #'car)
                                       (incf transition-index)
                                       (setf proposed-name (format nil "T~d" transition-index)))
                                     (funcall transition-label-cb transition-label proposed-name)
                                     (push (list transition-label proposed-name) transition-abrevs)
                                     proposed-name)))))             
	     new-name))
         (state-label-for-dot (state)
           (state-number state)))
    (format stream "digraph G {~%")
    (let ((*print-case* :downcase)
          (graph-label (make-string-output-stream))
	  (hidden 0)
          (states (sort (copy-list (states ndfa))
                        state<)))

      (when title
        (format graph-label "~a" title))
      (format stream "  rankdir=LR;~%")
      (format stream "  graph [labeljust=l,nojustify=true];~%")
      (format stream "  node [fontname=Arial, fontsize=25];~%")
      (format stream "  edge [fontname=Helvetica, fontsize=20];~%")
      
      (when (find-duplicates states :key #'state-label-for-dot :test #'equal)
        (error "multiple states with same label: ~A" (find-duplicates states :key #'state-label-for-dot :test #'equal)))
      (dolist (state states)
	(format stream "  /* ~Astate ~D ~A */~%"
                (if (state-sticky-p state) "sticky " "")
                (state-label-for-dot state)
                state)

        ;; incoming arrow to initial state
	(when (state-initial-p state)
	  (format stream "    H~D [label=\"\", style=invis, width=0]~%" hidden)
	  (format stream "    H~D -> ~D;~%" hidden (state-label-for-dot state))
	  (incf hidden))

	(format stream "  ~D [" (state-label-for-dot state))
        (when (state-sticky-p state)
          (format stream "style=dashed"))
	(format stream "]~%")
        
        ;; draw arrows from one state to the next for each transition.
	;; except if two arrows have the same source and destination,
	;; in which case draw one arrow with several comma separated labels.
	(destructuring-dolist ((next-state transitions) (group-by (transitions state)
                                                                  :key #'next-state :test #'eq))
          (let ((label-text (with-output-to-string (str)
			      (format str "~A" (new-transition-name (car transitions)))
			      (dolist (transition (cdr transitions))
				(format str ",~A" (new-transition-name transition))))))
	    (format stream "    ~D -> ~D [label=~S]~%"
                    (state-label-for-dot state)
		    (state-label-for-dot next-state)
                    label-text)))

	(cond
	  ((null (state-final-p state))
	   nil)
	  ((state-exit-form state)
	   (format stream "    X~D [label=\"~A\", shape=rarrow]~%" ;; or plaintext ?
		   hidden ;;(clause-index state)
                   (state-exit-form state)
                   )
	   (format stream "    ~D -> X~D ;~%" (state-label-for-dot state) hidden)
	   (incf hidden))
	  (t
	   (format stream "    H~D [label=\"\", style=invis, width=0]~%" hidden)
	   (format stream "    ~D -> H~D ;~%" (state-label-for-dot state) hidden)
	   (incf hidden))))

      (if report-labels
          (dolist (pair (sort (copy-list transition-abrevs) #'string-lessp :key #'cadr))
            (format t "~A -> ~A~%" (cadr pair) (car pair))))
      (format stream "}~%"))))

(defmethod ndfa-to-dot :around (ndfa (path string) &rest args)
  (apply #'ndfa-to-dot ndfa (pathname path) args))

(defmethod ndfa-to-dot :around (ndfa (path null) &rest args &key view prefix &allow-other-keys)
  "If NDFA-TO-DOT is called with NIL as second argument, and :VIEW T is given, then
the .dot file will be printed to a temporary file in /tmp (see MAKE-TEMP-FILE)."
  (if view ;; and path==nil
      (apply #'ndfa-to-dot ndfa
	     (make-temp-file-name (or prefix "dfa") :extension "png" :ensure-file-exists t)
	     args)
      (call-next-method)))

(defmethod ndfa-to-dot ((ndfa state-machine) (path pathname) &key (state-legend :dot) (transition-legend nil) transition-abrevs (transition-label-cb #'transition-label-cb) (view nil) prefix title (equal-transition-labels #'equal) (state< #'ndfa-state<) (report-labels nil))
  "Calling NDFA-TO-DOT with a PATH whose type is \"dot\" creates the dot file, which is valid input for the
graphviz dot program.   If PATH has type \"png\", a temporary dot file will be created, and
will be converted to a png file which will be displayed using open -n.  This works for MAC only."
  (declare (type (function (t t) t) transition-label-cb))
  (cond ((string= "dot" (pathname-type path))
         (ensure-directories-exist path)
	 (with-open-file (stream path :direction :output :if-exists :rename)
	   (ndfa-to-dot ndfa stream :state-legend state-legend :transition-legend transition-legend :transition-abrevs transition-abrevs :transition-label-cb transition-label-cb :view nil :prefix prefix :title title :equal-transition-labels equal-transition-labels :state< state< :report-labels report-labels)))
	((string= "png" (pathname-type path))
	 (let ((dotpath (merge-pathnames (make-pathname :type "dot")  path)))
	   (ndfa-to-dot ndfa dotpath :state-legend state-legend :transition-legend transition-legend :transition-abrevs transition-abrevs :transition-label-cb transition-label-cb :view nil :prefix prefix :title title :equal-transition-labels equal-transition-labels :state< state< :report-labels report-labels)
	   (run-program *dot-path* (list "-Tpng" (namestring dotpath) "-o" (namestring path)))
	   #+:os-macosx (when view (run-program "open" (list "-n" (namestring path))))))
	(t
	 (error "invalid path ~A" path))))
