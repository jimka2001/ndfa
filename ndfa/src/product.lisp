;; Copyright (c) 2019 EPITA Research and Development Laboratory
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

(defgeneric populate-synchronized-product (sm-product sm1 sm2 &key boolean-function minimize merge-transition-labels complement-transition-label)
  (:documentation
   "Given two state machines for which we wish to calculate the cross-product,
 and a product state machine which has be allocated (as if by make-instance), update the product state machine
 with the states comprising the synchronized product.  There are several optional arguments:
 :MINIMIZE -- boolean, (default TRUE) whether to minimize the newly created automaton.
 :BOOLEAN-FUNCTION -- This function is used to determine whether a newly created state (in the synchronized 
   product) needs to be a final state.  If the synchronized product is being calculated to compute the AND of
   two state machines then the BOOLEAN-FUNCTION should be (lambda (a b) (and a b)).
 :MERGE-TRANSITION-LABELS - when calculating the cross product of two state, we must examine
   every possible pairing of transitions from two given states.  This function, MERGE-TRANSITION-LABELS, is
   called on two such labels to calculate the label for the transition to the new state in the
   cross-product machine.
 :COMPLEMENT-TRANSITION-LABEL - given a state, return a transition label that represents
   all the transitions to the sync state.
 :FINAL-STATE-CALLBACK -- a callback function to be called each time a new final state in the synchronized 
   product has been added, and after the transitions have been added, but before the state machine
   has been trimmed and reduced."))

(defmethod populate-synchronized-product ((sm-product state-machine)
					  (sm1 state-machine)
					  (sm2 state-machine)
					  &key (boolean-function (lambda (a b) (and a b)))
                                            (minimize t)
                                            (merge-transition-labels
                                             (lambda (label-1 label-2)
                                               (error "need to merge ~A and ~A, no merge-transition-labels given to populate-synchronized-product"
                                                      label-1 label-2)))
                                            (complement-transition-label
                                             (lambda (state)
                                               (error "need to calculate complement transition for ~A, no complement-transition-label given to populate-synchronized-product" state)))
					    (final-state-callback (lambda (product-final-state final-state-1 final-state-2)
								    (declare (ignore product-final-state final-state-1 final-state-2))
								    nil)))
  (declare (type (function (t t) t) merge-transition-labels boolean-function)
           (type (function ((or state null)) t) complement-transition-label)
	   (type (function ((or null state) (or null state) (or null state)) t) final-state-callback)
	   (optimize (speed 3) (debug 0) (compilation-speed 0)))
  (let ((label 0)
	(states->state (make-hash-table :test #'equal))
	(state->states (make-hash-table :test #'eq))
	(buf (list nil)))
    (labels ((calc-final (st1 st2)
	       (declare (type (or null state) st1 st2))
	       (funcall boolean-function (and st1 (state-final-p st1)) (and st2 (state-final-p st2))))
	     (calc-clause-index (st1 st2)
	       (declare (type (or null state) st1 st2))
	       (cond
		 ((null st1)
		  (clause-index st2))
		 ((null st2)
		  (clause-index st1))
		 ((null (clause-index st2))
		  (clause-index st1))
		 ((null (clause-index st1))
		  (clause-index st2))
		 (t
		  (min (clause-index st1) (clause-index st2)))))
	     (product-state (st1 st2 &key initial-p)
	       (declare (type (or null state) st1 st2))
	       (or (gethash (list st1 st2) states->state nil)
		   (let ((new-state (add-state sm-product
					       :clause-index (calc-clause-index st1 st2)
					       :initial-p initial-p
					       :final-p (calc-final st1 st2)
					       :label (incf label))))
		     (setf (gethash (list st1 st2) states->state) new-state
			   (gethash new-state state->states) (list st1 st2))
		     (tconc buf new-state)
		     new-state)))
	     (make-initial-states ()
	       (dolist (st1 (get-initial-states sm1))
		 (dolist (st2 (get-initial-states sm2))
		   (product-state st1 st2 :initial-p t)))))
      
      (make-initial-states)
      (dolist-tconc (product-state buf)
        (format t "~&-------------------------~%")
        (format t "~A~%" product-state)
	(destructuring-bind (st1-from st2-from) (gethash product-state state->states)
          (let ((transitions1 (and st1-from (transitions st1-from)))
                (transitions2 (and st2-from (transitions st2-from))))
            (dolist (transition-1 (cons nil transitions1))
              (dolist (transition-2 (cons nil transitions2))
                (let ((transition-label-1 (if (null transition-1)
                                              (funcall complement-transition-label st1-from)
                                              (transition-label transition-1)))
                      (transition-label-2 (if (null transition-2)
                                              (funcall complement-transition-label st2-from)
                                              (transition-label transition-2))))
                  (let ((new-transition-label (funcall merge-transition-labels transition-label-1 transition-label-2))
                        (next-state-1 (and transition-1 (next-state transition-1)))
                        (next-state-2 (and transition-2 (next-state transition-2))))
                    ;; Is it possible that the same new-transition-label appears twice?
                    ;; No, unless it is nil.
                    ;; Why?  Because transition-labels-1 is a list of disjoint types,
                    ;;           and transition-labels-2 is a list of disjoint types.
                    ;;       Suppose A,B are in transitions-labels-1,
                    ;;       and     X,Y are in transitions-labels-2.
                    ;;       AX is a subset of A, and BY is a subset of B
                    ;;       and A and B are disjoint; therefore AX and BY are disjoint.
                    ;;       Note that the empty set is disjoint from itself, so it might happen that
                    ;;       AX = BY = nil.
                    (when (and new-transition-label
                               (or next-state-1
                                   next-state-2))
                      (let ((next-product-state (product-state next-state-1 next-state-2)))
                        (format t " transition=~A --> ~A~%" new-transition-label next-product-state)

                        (add-transition product-state :next-label (state-label next-product-state)
                                                      :transition-label new-transition-label))))))))))

      (dolist (state (states sm-product))
        (dolist (transition (transitions state))
          (format t "~A --> ~A~%" state (next-state transition))))
      
      (dolist (final-state (get-final-states sm-product))
	(destructuring-bind (st1-from st2-from) (gethash final-state state->states)
	  (funcall final-state-callback final-state st1-from st2-from)))

      (when minimize
        (minimize-state-machine sm-product))

      (calc-sticky-states sm-product)
      sm-product
      )))

(defgeneric synchronized-product (sm1 sm2 &key boolean-function minimize))

(defmethod synchronized-product ((sm1 state-machine) (sm2 state-machine) &key (minimize t) (boolean-function (lambda (a b) (and a b))))
  (declare (optimize (speed 3) (debug 0) (compilation-speed 0))
	   (type (function (t t) t) boolean-function))
  (if (eq (class-of sm1)
	  (class-of sm2))
      (populate-synchronized-product (make-instance (class-of sm1)) sm1 sm2
                                     :boolean-function boolean-function
                                     :minimize minimize)
      (error "Cannot create synchronized product of ~A and ~A" (class-of sm1) (class-of sm2))))
