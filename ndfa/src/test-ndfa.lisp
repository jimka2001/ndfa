;; Copyright (c) 2016 EPITA Research and Development Laboratory
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


(defpackage :ndfa-test
  (:use :cl :ndfa :scrutiny :adjuvant))

(in-package :ndfa-test)

(defun test ()
  (run-package-tests :ndfa-test))

(define-test ndfa/test-reduce-2
  (let* ((dfa (make-ndfa '((:label x0 :initial-p t
			   :transitions ((:next-label x6 :transition-label t1)
					 (:next-label x1 :transition-label t2)))
			  (:label x4 :final-p t)
			  (:label x1
			   :transitions ((:next-label x2 :transition-label t3)))
			  (:label x6
			   :transitions ((:next-label x2 :transition-label t3)))
			  (:label x2
			   :transitions ((:next-label x3 :transition-label t4)
					 (:next-label x5 :transition-label t5)))
			  (:label x3
			   :transitions ((:next-label x4 :transition-label t6)))
			  (:label x5
			   :transitions ((:next-label x4 :transition-label t6))))))
	 (reduced-dfa (reduce-state-machine dfa)))
    (assert-true (= 7 (length (states dfa))))
    (assert-true (= 5 (length (states reduced-dfa))))

))

(define-test ndfa/test-reduce-3
  (let* ((dfa (make-ndfa '((:label A
			    :initial-p t
			    :transitions ((:next-label B :transition-label 0)
					  (:next-label C :transition-label 1)))
			   (:label B
			    :transitions ((:next-label B :transition-label 0)
					  (:next-label D :transition-label 1)))
			   (:label C
			    :transitions ((:next-label B :transition-label 0)
					  (:next-label C :transition-label 1)))
			   (:label D
			    :transitions ((:next-label B :transition-label 0)
					  (:next-label E :transition-label 1)))
			   (:label E
			    :final-p t
			    :transitions ((:next-label B :transition-label 0)
					  (:next-label C :transition-label 1))))))
	 (reduced-dfa (reduce-state-machine dfa)))
    (assert-true (= 4 (length (states reduced-dfa))))
    ))
  

(define-test ndfa/test-trim-1
  (let ((dfa (make-ndfa '((:label a :initial-p t)
			  (:label b :final-p t)))))
    (assert-true (= 0 (length (states (trim-state-machine dfa))))))  )

(define-test ndfa/test-trim-2

  (let ((dfa (make-ndfa '((:label i :initial-p t :transitions ((:next-label m :transition-label 1)))
			  (:label m :transitions ((:next-label f :transition-label 1)))
			  (:label f :final-p t)))))
    (assert-true (= 3 (length (states (trim-state-machine dfa))))))

  )

(define-test ndfa/test1
  (let ((sm (make-instance 'state-machine :key #'evenp)))
    (add-state sm :label 'a
		  :initial-p t
		  :transitions `((:next-label b :transition-label t)
				 (:next-label c :transition-label nil)))
    (assert-true (states sm))
    (add-state sm :label 'b
		  :final-p t
		  :initial-p t
		  :transitions `((:next-label c :transition-label t)
				 (:next-label b :transition-label nil)))
    (add-state sm :label 'c
		  :transitions `((:next-label b :transition-label t)
				 (:next-label c :transition-label nil)))
    (with-output-to-string (str)
      (dolist (state (states sm))
	(dolist (transition (transitions state))
	  (princ (next-state transition) str))))
    (mapcar #'transitions (states sm))
    (assert-true (get-initial-states sm))
    (assert-true (= 2 (length (get-initial-states sm))))

    (assert-true (get-initial-states sm))
    (perform-transitions sm '(1))
    (perform-transitions sm '(1 2))
    (perform-transitions sm #(1 2 3))
    ))


(define-test ndfa/test2
  (let ((sm (make-ndfa `((:label a
			  :initial-p t
			  :transitions ((:next-label b :transition-label 0)
					(:next-label c :transition-label 1)))
			 (:label b
			  :final-p t
			  :initial-p t
			  :transitions ((:next-label c :transition-label 0)
					(:next-label b :transition-label 1)))
			 (:label c
			  :transitions ((:next-label b :transition-label 0)
					(:next-label c :transition-label 1))))
		       :test (lambda (label input-element)
			       (or (and (evenp label) (evenp input-element))
				   (and (oddp  label) (oddp input-element)))))))
  
    (with-output-to-string (str)
      (dolist (state (states sm))
	(dolist (transition (transitions state))
	  (princ (next-state transition) str))))
    (mapcar #'transitions (states sm))
    (assert-true (get-initial-states sm))
    (assert-true (= 2 (length (get-initial-states sm))))
    (assert-true (get-initial-states sm))
  
    (perform-transitions sm '(1))
    (perform-transitions sm '(1 2))
    (perform-transitions sm #(1 2 3 4))
    ))
