;; Copyright (c) 2018 EPITA Research and Development Laboratory
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

(defun remove-non-coaccessible-states (dfa)
  "Remove all states from the state machine which have no path to a final state"
  (declare (type state-machine dfa))
  (let ((buf (list nil))
	reverse-assoc)
    (dolist (f (get-final-states dfa))
      (tconc buf f))

    ;; build (back-pointerss) an alist which maps a state to all the
    ;;          states which have a transition to it.
    ;;   car --> target state
    ;;   cdr --> list of states with a transition to target 
    (dolist (state (states dfa))
      (dolist (transition (transitions state))
	(let ((target (next-state transition)))
	  (if (assoc target reverse-assoc)
	      (pushnew state (cdr (assoc target reverse-assoc)))
	      (push (list target state) reverse-assoc)))))

    ;; now, using the back-pointers, trace back from final states
    ;;   to all states which have a path thereto.
    (dolist-tconc (target buf)
      (dolist (before (cdr (assoc target reverse-assoc)))
	(unless (member before (car buf))
	  (tconc buf before))))

    ;; a state in a state machine is called co-accessible if there is
    ;; a path from it to a final state.
    ;; We have collected in (car buf) the list of co-accessible states.
    ;; So states not in (car buf) are not co-accessible.
    ;; Now we remove such non-co-accessible states from (states dfa)
    ;; from (get-final-states dfa) and also from (get-initial-states dfa).
    (remove-invalid-states dfa (car buf))))

(defun remove-non-accessible-states (dfa)
  "Remove all states from the state machine which no path from any initial state."
  (declare (type state-machine dfa))
  ;; first we find all the accessible states by tracing from the initial states as far as possible.
  ;; everything not collected this way is non-accessible.
  (let ((buf (list nil)))
    (dolist (f (get-initial-states dfa))
      (tconc buf f))
    (dolist-tconc (source-state buf)
      (dolist (transition (transitions source-state))
	(unless (member (next-state transition) (car buf))
	  (tconc buf (next-state transition)))))
    ;; a state in a state machine is called accessible if there is
    ;; a path from some initial state to it.
    ;; We have collected in (car buf) the list of accessible states.
    ;; So states not in (car buf) are not accessible.
    ;; Now we remove such non-accessible states from (states dfa)
    ;; from (get-final-states dfa) and also from (get-initial-states dfa).
    (remove-invalid-states dfa (car buf))))

(defun trim-state-machine (dfa)
  "Trim a state machine.  This means if any state has no path to a final state, then remove
it; and if any state is not reachable from an inital state, then remove it.
RETURNS the given DFA perhaps after having some if its states removed."
  ;; we have to remove non-coaccessible states before remove-non-accessible-states,
  ;;    because the function REMOVE-INVALID-STATES checks that removing states does not
  ;;    create non-coaccessible.  If we called the functions in the opposite order
  ;;    there might be non-coaccessible states after removing the non-accessible states.
  (remove-non-coaccessible-states dfa)
  (remove-non-accessible-states dfa))

(defun minimize-state-machine (dfa &key (combine (transition-label-combine dfa)) (equal-labels (transition-label-equal dfa)))
  ":COMBINE -- either nil or a binary function which takes two transition labels and returns a new label representing
 the combination of the two given.
 :EQUAL-LABELS -- equivalence function for transition labels."
  (declare (type state-machine dfa)
	   (type (or null (function (t t) t)) combine))
  (trim-state-machine dfa)
  (let ((partitions (cons (set-difference (states dfa) (get-final-states dfa))
			  (mapcar #'cadr (group-by (get-final-states dfa) :key #'state-exit-form :test #'equal)))))
    ;; initialize PARTITIONS by 1. the set of states which are NOT
    ;;    final, and 2...n sets of states which are final where each
    ;;    subset has the same (EQUAL) value of state-exit-form.
    (labels ((find-partition (state)
	       (declare (type state state))
	       (the cons
		    (car (exists partition partitions
			   (member state partition :test #'eq)))))
	     (partition-transition (state)
	       (declare (type state state))
	       (mapcar (lambda (transition)
			 (declare (type transition transition))
			 (list  :with (transition-label transition)
				:to (find-partition (next-state transition))))
		       (transitions state)))
	     (plist-equal (plist1 plist2)
	       (declare (type (cons keyword cons) plist1 plist2))
	       (and (eq (getf plist1 :to)
			(getf plist2 :to))
		    (funcall equal-labels
			     (getf plist1 :with)
			     (getf plist2 :with))))
	     (refine-partition (partition)
	       ;; Partition is a set (list of states) into one or more
	       ;; lists such that each resulting list has the property
	       ;; that all its element have the same value of
	       ;; PARTITION-TRANSITION.  Here 'same' mean contains the
	       ;; same elements in some order. (NOT (SET-EXCLUSIVE-OR
	       ;; ...))
	       ;; 
	       (let ((characterization (group-by partition
						 :key #'partition-transition
						 :test (lambda (plists1 plists2)
							 (not (set-exclusive-or plists1 plists2
										:test #'plist-equal))))))
		 ;; characterization is a car/cadr alist mapping a
		 ;; plist to a list of states which is a subset of
		 ;; partition plist looks like ( :with ...  :to ...)
		 (loop :for grouped-by-transitions :in characterization
		       :collect (destructuring-bind (_plist equiv-states) grouped-by-transitions
				  (declare (ignore _plist))
				  ;; this call to SETOF creates a list with the same elements as equiv-states,
				  ;;   but so that they are order in the same order as they are found
				  ;;   in (STATES DFA).  This is so that FIXED-POINT can depend on the order
				  ;;   and recognize when the same value has been returned twice from REFINE-PARTITIONS.
				  (setof state (states dfa)
				    (member state equiv-states :test #'eq))))))
	     (min-clause-index (v1 v2)
	       (cond ((null v1) v2)
		     ((null v2) v1)
		     (t
		      (min v1 v2))))
	     (refine-partitions (p)
	       (setf partitions (mapcan #'refine-partition p))))

      ;; Generate the partition by calling REFINE-PARTITIONS until we
      ;;   find a fixed-point each such iteration modifies the value
      ;;   of PARTITIONS.  each such refinement breaks one or more of
      ;;   the subsets into smaller subsets.  This partitioning
      ;;   assures that given any valid transition label L, and given
      ;;   a partitions P=(p.1, p.2, ... p.n) (where p.1 .. pn are
      ;;   states), then when p.i and p.j are in P then L take p.i and
      ;;   p.j to the same partition.  I.e., if L(p.i)=q.i and
      ;;   L(p.j)=q.j, then q.i and q.j are in the same partition.  By
      ;;   'valid' transition we mean that it is a transition leaving
      ;;   some state in P.
      
      (fixed-point #'refine-partitions
		   partitions)

      ;; Now build new state machine, and combine any parallel
      ;; transitions using the COMBINE function.
      (let* ((reduced-dfa (make-instance (class-of dfa)))
	     (new-state->equiv-class 
	       ;; add new states to reduced-dfa
	       (loop :for equiv-class :in partitions
		     :collect (cons (add-state reduced-dfa
					       :label (state-label (car equiv-class))
					       :initial-p (if (exists state equiv-class
								(state-initial-p state))
							      t nil)
					       :clause-index (reduce #'min-clause-index equiv-class :key #'clause-index)
					       :exit-form (state-exit-form (car equiv-class))
					       :final-p   (if (exists state equiv-class
								(state-final-p state))
							      t nil))
				    equiv-class))))
	
	;; add transitions to each state
	(loop :for (from-state . from-equiv-class) :in new-state->equiv-class
	      ;; collect all the transistions from this from-equiv-class
	      ;; and group them by equiv-class of next state
	      :for transitions = (mapcan (lambda (old-state)
					   (copy-list (transitions old-state))) from-equiv-class)
	      :for grouped-by-destination = (group-by transitions
						      :key (lambda (transition)
							     (find-partition (next-state transition)))
						      :test #'eq)
	      :do (loop :for pair :in grouped-by-destination
			:for to-equiv-class = (car pair)
			:for to-label = (state-label (car to-equiv-class))
			:for transitions = (remove-duplicates (cadr pair)
							      :key #'transition-label
							      :test equal-labels)
			:do (if combine
				;; duplicate transition labels have already been removed, but
				;;   we still need to compile labels which are different.
				;;   e.g., number + string = (or number string)
				;;   e.g., fixnum + (and number (not fixnum)) = number
				(add-transition from-state
						:equal-label equal-labels
						:transition-label (reduce combine (mapcar #'transition-label transitions)
									  :initial-value (transition-label (car transitions)))
						:next-label to-label)
				;; otherwise, make several transitions between the same two states,
				;; each with a different transition label
				(dolist (transition transitions)
				  (add-transition from-state
						  :equal-label equal-labels
						  :transition-label (transition-label transition)
						  :next-label to-label)))))
	reduced-dfa
	))))

(defgeneric populate-synchronized-product (sm-product sm1 sm2 &key boolean-function union-labels match-label)
  (:documentation
   "Given two state machines for which we wish to calculate the cross-product,
 and a product state machine which has be allocated (as if by make-instance), update the product state machine
 with the states comprising the synchronized product.  There are several optional arguments:
 :BOOLEAN-FUNCTION -- This function is used to determine whether a newly created state (in the synchronized 
   product) needs to be a final state.  If the synchronized product is being calculated to compute the AND of
   two state machines then the BOOLEAN-FUNCTION should be (lambda (a b) (and a b)).
 :UNION-LABELS -- This function is used to calculate the list of transition labels for a newly
   created state in the synchronized product, given the list of transition labels from the original two
   states.  This defaults to #'union.  This option is important for the method of 
   POPULATE-SYNCHRONIZED-PRODUCT specializing on RTE-STATE-MACHINE which uses a curried form of
   #'MDTD-BDD.
 :MATCH-LABEL -- Find a transition given a label.  This function will be called with two arguments in
   a dependable order; 1st the label being sought, 2nd the label of one of the existing transitions.
   The ability to search for a transition with a predicate other than equality is important for the 
   method of POPULATE-SYNCHRONIZED-PRODUCT specializing on RTE-STATE-MACHINE which uses #'subtypep
   for this predicate.
 :FINAL-STATE-CALLBACK -- a callback function to be called each time a new final state in the synchronized 
   product has been added, and after the transitions have been added, but before the state machine
   has been trimmed and reduced."))

(defmethod populate-synchronized-product ((sm-product state-machine)
					  (sm1 state-machine)
					  (sm2 state-machine)
					  &key (boolean-function (lambda (a b) (and a b)))
					    (union-labels #'union)
					    (match-label #'eql)
					    (final-state-callback (lambda (product-final-state final-state-1 final-state-2)
								    (declare (ignore product-final-state final-state-1 final-state-2))
								    nil)))
  (declare (type (function (t t) t) boolean-function)
	   (type (function (list list) list) union-labels)
	   (type (function ((or null state) (or null state) (or null state)) t) final-state-callback)
	   (type (function (t t) t) match-label)
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
	     (match-transition (st-from label)
	       (find-if (lambda (transition)
			  (funcall match-label label (transition-label transition)))
			(transitions st-from)))
	     (match-next-state (st-from label &aux (matching-transition (and st-from (match-transition st-from label))))
	       ;; matching-transition is either a transition object or nil
	       (cond
		 ((null st-from) nil)
		 (matching-transition
		  (next-state matching-transition))
		 (t
		   nil)))
	       
	     (make-initial-states ()
	       (dolist (st1 (get-initial-states sm1))
		 (dolist (st2 (get-initial-states sm2))
		   (product-state st1 st2 :initial-p t)))))
      
      (make-initial-states)
      (dolist-tconc (product-state buf)
	(destructuring-bind (st1-from st2-from) (gethash product-state state->states)
	  (dolist (transition-label (funcall union-labels
					     (and st1-from (mapcar #'transition-label (transitions st1-from)))
					     (and st2-from (mapcar #'transition-label (transitions st2-from)))))
	    (let* ((next-state-1 (match-next-state st1-from transition-label))
		   (next-state-2 (match-next-state st2-from transition-label))
		   (next-product-state (product-state next-state-1 next-state-2)))
	      (add-transition product-state :next-label (state-label next-product-state) :transition-label transition-label)))))

      (dolist (final-state (get-final-states sm-product))
	(destructuring-bind (st1-from st2-from) (gethash final-state state->states)
	  (funcall final-state-callback final-state st1-from st2-from)))

      (minimize-state-machine sm-product))))

(defgeneric synchronized-product (sm1 sm2 &key boolean-function))

(defmethod synchronized-product ((sm1 state-machine) (sm2 state-machine) &key (boolean-function (lambda (a b) (and a b))))
  (declare (optimize (speed 3) (debug 0) (compilation-speed 0))
	   (type (function (t t) t) boolean-function))
  (if (eq (class-of sm1)
	  (class-of sm2))
      (populate-synchronized-product (make-instance (class-of sm1)) sm1 sm2 :boolean-function boolean-function)
      (error "Cannot create synchronized product of ~A and ~A" (class-of sm1) (class-of sm2))))
