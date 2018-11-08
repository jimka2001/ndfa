;; Copyright (c) 2016,2018 EPITA Research and Development Laboratory
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

(defpackage :non-deterministic-finite-automata
  (:use :cl :adjuvant)
  (:nicknames "NDFA")
  (:export "ADD-STATE"
	   "DETERMINISTICP"
	   "GET-FINAL-STATES"
	   "GET-INITIAL-STATES"
	   "GET-STICKY-STATES"
	   "NDFA-TO-DOT"
	   "NEXT-LABEL"
	   "NEXT-STATE"
	   "PERFORM-SOME-TRANSITIONS"
	   "PERFORM-TRANSITIONS"
	   "POPULATE-SYNCHRONIZED-PRODUCT"
	   "MINIMIZE-STATE-MACHINE"
	   "STATE-EXIT-FORM"
	   "STATE-FINAL-P"
	   "STATE-LABEL"
	   "STATE-MACHINE"
	   "STATE-NAME"
	   "STATE-STICKY-P"
	   "STATES"
	   "SYNCHRONIZED-PRODUCT"
	   "TRANSITION-LABEL-COMBINE"
	   "TRANSITION-LABEL-EQUAL"
	   "TRANSITION-LABEL"
	   "TRANSITIONS"
	   "TRIM-STATE-MACHINE"
	   "MAKE-NDFA" ))

(in-package :ndfa)

(defclass state-machine ()
  ((states :initarg :states :initform nil :accessor states
	   :documentation "List of elements of class STATE")
   (deterministicp :initform nil :initarg :deterministicp :accessor deterministicp)
   (final-states :accessor get-final-states)
   (sticky-states :accessor get-sticky-states)
   (initial-states :accessor get-initial-states
		   :documentation "Subset of the STATES slot indicating which of the states are initial,
 ie. which of the states answer TRUE to the STATE-INITIAL-P predicate.")
   (test :initform #'eql :initarg :test :reader test :type (function (t t) t)
	 :documentation ":KEY :TEST are the idiomatic key/test pair common to many
lisp functions. The function PERFORM-SOME-TRANSITIONS uses these two function to
iteratively determine whether each element of its INPUT sequence is
accepted or rejected. 
:TEST designates a binary predicate to be called two arguments:
   input-data - Each element of the INPUT-SEQUENCE is passed to the :KEY
                function and that value is passed as the 1st argument of
                the :TEST function.
   transition-label - the value returned from (TRANSITION-LABEL transition)
                is passed as the 2nd argument of the :TEST function")
   (key  :initform #'identity :initarg :key :reader key :type (function (t) t))
   (transition-label-combine :initform nil :initarg :transition-label-combine
			     :reader transition-label-combine
			     :type (or null (function (t t) t))
			     :documentation "When reducing a state machine, this function
 takes two transition labels and returns a new label representing
 the combination of the two given. nil => don't combine parallel transitions.")
   (transition-label-equal :initform #'eql :initarg :transition-label-equal
			   :type (function (t t) t)
			   :reader transition-label-equal
			   :documentation "When reducing a state-machine, this function indicates 
how to determine whether two transition labels are considered equal."))
  (:documentation "A finite state machine.  An application program is expected to maintain
a list of states, each an element of (STATES ...), and use either the function
PERFORM-TRANSITIONS or PERFORM-SOME-TRANSITIONS to compute the list of next states
given an INPUT-SEQUENCE.  Use the factory function, MAKE-NDFA, to create an instance
of STATE-MACHINE. The list of states of the machine may be specified as the
:STATES argument to MAKE-NDFA, by subsequent calls to ADD-STATE."))

(defvar *state-number* 0)

(defclass state ()
  ((label :initarg :label :reader state-label
	  :documentation "An object, usually a number, string, symbol, list indentifying this state.
Code which manipulate state transitions, use this label to identify intended states before the states
have yet been created as part of the initialization process.  The label is also used within PRINT-OBJECT.
It is not allowed to have two different states in the same state-machine which have the same label
according to the EQUAL function.")
   (state-number :initform (incf *state-number*) :reader state-number)
   (ndfa :initarg :ndfa :reader ndfa :type state-machine
	 :documentation "The instance of STATE-MACHINE for which this instance of STATE is state of.  I.e.,
this STATE instance is a member of (STATES (STATE-MACHINE state))")
   (transitions :type list :initform nil :accessor transitions
		:documentation "List of instances of class TRANSITION")
   (initial-p :initarg :initial-p :initform nil :reader state-initial-p
	      :documentation "Indicates whether the state is an initial state of the state machine.")
   (sticky-p :initarg :sticky-p :initform nil :accessor state-sticky-p
	     :documentation "A state is sticky if once the NDFA gets into this state, it cannot leave.")
   ;; TODO, not sure this is the best place to put the continuation information
   (exit-form :initarg :exit-form :accessor state-exit-form)
   (final-p :initarg :final-p :initform nil :reader state-final-p
	    :documentation "Indicates whether the state is a final state of the state machine."))
  (:documentation "Instances of this class comprise the values of the STATES slot of
an instance of class STATE-MACHINE."))

(defmethod slot-unbound ((class standard-class) (state state) (slot-name (eql 'exit-form)))
  (setf (slot-value state slot-name)
	(if (state-final-p state)
	    t
	    nil)))

(defgeneric state-name (state))

(defmethod state-name ((state state))
  ;; (intern (with-output-to-string (str)
  ;; 	    (write (state-label state)
  ;; 		   :stream str
  ;; 		   :pretty nil
  ;; 		   :escape t))
  ;; 	  (symbol-package 'state-name))
  (slot-value state 'state-number)
  )

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'initial-states)))
  "Calculate the list of initial states and setf it as the value of the INITIAL-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-initial-p state)
						  (list state)))
					    (states ndfa))))

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'sticky-states)))
  "Calculate the list of initial states and setf it as the value of the STICKY-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-sticky-p state)
						  (list state)))
					    (states ndfa))))

(defmethod slot-unbound ((class standard-class) (ndfa state-machine) (slot-name (eql 'final-states)))
  "Calculate the list of initial states and setf it as the value of the FINAL-STATES slot."
  (setf (slot-value ndfa slot-name) (mapcan #'(lambda (state)
						(when (state-final-p state)
						  (list state)))
					    (states ndfa))))

(defmethod print-object ((self state) stream)
  (if (slot-boundp self 'label)
      (print-unreadable-object (self stream :type t :identity nil)
	(format stream "~A" (state-label self))
	(when (state-initial-p self)
	  (format stream "[I]"))
	(when (state-final-p self)
	  (format stream "[F]")))
	
      (call-next-method)))

(defclass transition ()
  ((state :initarg :state :type state :accessor state)
   (next :initarg :next :type (or nil state) :reader next-state) ; initialized lazily based on value of next-label
   (transition-label :initarg :transition-label :accessor transition-label
		     :documentation "An object such as number, string, symbol, list which designates the
test controlling a transition to the state indicated by NEXT-LABEL.  For more details see
the documentation of the TEST and KEY slots of the STATE-MACHINE class.")
   (next-label :initarg :next-label :reader next-label
	       :documentation "A state label, indicating that this transition object represents a 
state machine transition from the state STATE to the state whose LABEL is NEXT-LABEL. NEXT-LABEL
is used in the initialization process.  Its value might indicate a state which has not yet been created.
After all the transitions to all the states have been added, it is expected that each NEXT-LABEL is EQUAL
to the LABEL of some state in the state-machine.  However, care should be taken during initialization not
to assume that such a state already exists.  After initialization is finished, the NEXT slot will be the
state object whose STATE-LABEL is EQUAL to this NEXT-LABEL"))
  (:documentation "Instances of this class comprise the list in the TRANSITIONS slot of the class STATE."))

(defmethod initialize-instance :after ((self transition) &rest initargs)
  (unless (slot-boundp self 'transition-label)
    (error "~A was created with no transition-label.  initargs=~A" self initargs)))

(defmethod ndfa ((self transition))
  (ndfa (state self)))

(defmethod print-object ((self transition) stream)
  (if (and (slot-boundp self 'state)
	   (slot-boundp (state self) 'label)
	   (slot-boundp self 'next-label))
      (print-unreadable-object (self stream :type t :identity nil)
	(format stream "~A->~A" (state-label (state self)) (next-label self)))
      (call-next-method)))

(defmethod slot-unbound ((class standard-class) (self transition) (slot-name (eql 'next)))
  "It is expected that NEXT-LABEL indicates the LABEL of some state in the state machine.
This method lazily sets the NEXT slot of SELF to the STATE slot to that state."
  (let ((state (find (next-label self) (states (ndfa self)) :key #'state-label :test #'equal)))
    (if state
	(setf (slot-value self slot-name) state)
	(error "transition has next-label=~A indicating non-existing state: available labels are: ~A"
	       (next-label self)
	       (mapcar #'state-label (states (ndfa self)))))))

(defgeneric perform-some-transitions (sm starting-states input-sequence))
(defmethod perform-some-transitions ((ndfa state-machine) starting-states input-sequence)
  "Given a list of STARTING-STATES, each of which is an element of (states NDFA), 
perform the transitions indicated by INPUT-SEQUENCE, i.e., iterate through the
STARTING-STATES, and on each element generate a list of next-states, as a function of
the existing transitions on the state.  Some of the transitions will lead to a next state
and some won't; collect a list of such 'successful' next-states.  On this new list of states,
perform the same algorithm on the next element of the INPUT-SEQUENCE, and repeat until the
INPUT-SEQUENCE is depleted.  Return the list of resulting states.
There is a notable exception, if there is every a transition into a state which is both final
and sticky, then a singleton list of that state is returned, and no further transition, 
nor no further element fo the input-sequence is considered."
  (declare (type list starting-states)
	   (type sequence input-sequence))
  (let ((current-states starting-states)
	(deterministicp (deterministicp ndfa)))
    ;; We specifically use MAP here becasue it works on sequences,
    ;; rather than simply on lists.
    (map nil
	 (lambda (input &aux new-states)
	   (dolist (state current-states)
	     (block do-transitions
	       (dolist (transition (transitions state))
		 (when (funcall (test ndfa) (funcall (key ndfa) input) (transition-label transition))
		   (let ((next-state (next-state transition)))
		     (if (and (state-sticky-p next-state)
			      (state-final-p next-state))
			 (return-from perform-some-transitions (list next-state))
			 (progn (pushnew next-state new-states)
				(when deterministicp
				  ;; if ndfa is deterministic, we need only find one transition
				  (return-from do-transitions)))))))))
	   ;; if current-states is nil, EVERY will return
	   (setf current-states new-states))
	   input-sequence)
    (the list current-states)))

(defgeneric perform-transitions (sm input-sequence))

(defmethod perform-transitions ((ndfa state-machine) input-sequence)
  "Returns a list of states which are reached by the following process:
Start with the list/set of all initial states of the state-maching NDFA.
Iterate through the INPUT-SEQUENCE, performing all the applicable transitions.
If the list of states becomes empty, return NIL.
Otherwise the set of states reached is returned.
None, some, or all of these states might be final states of the state machine."
  (declare (type sequence input-sequence))
  (perform-some-transitions ndfa (get-initial-states ndfa) input-sequence))

(defgeneric add-transition (state &key next-label transition-label equal-label))

(defmethod add-transition ((state state) &key next-label transition-label (equal-label #'eql))
  "Create and return an instance of TRANSITION from STATE to the state designated by NEXT-LABEL.
Note, that the state indicated by NEXT-LABEL might not yet exist."
  (cond
    ;; if such a transition already exists, then just return it without creating a new one
    ((find-if (lambda (transition)
		(and (eql (next-label transition) next-label)
		     (funcall equal-label transition-label (transition-label transition))))
	      (transitions state)))
    ;; if a transition already exists the label then error
    ((exists transition (transitions state)
       (funcall equal-label transition-label (transition-label transition)))
     (error "a transition already exists with label ~A~%" transition-label))
    (t
     (car (push (make-instance 'transition :state state :next-label next-label :transition-label transition-label)
		(transitions state))))))

(defgeneric add-state (object &key label initial-p final-p transitions exit-form))

(defmethod add-state ((ndfa state-machine) &key label initial-p final-p transitions exit-form)
  "Add or update a state designated by the given LABEL.  If the state already exists
 in the state-machine NDFA, (whose STATE-LABEL is EQUAL to LABEL) its INITIAL-P and 
 FINAL-P are updated to TRUE if :INITIAL-P or :FINAL-P are given as such
 (but not updated to NIL).  If the state does not yet exists, it one is created
 and added to the state machine."
  ;; TRANSITIONS is a list of sublists, each sublist is of the form
  ;; (unary-test-function destination-label)
  (let ((existing-state (find label (states ndfa) :key #'state-label :test #'equal)))
    (cond
      (existing-state
       (when initial-p
	 (setf (slot-value existing-state 'initial-p) t))
       (when final-p
	 (setf (slot-value existing-state 'final-p) t))
       existing-state)
      (t
       (let ((new-state (make-instance 'state
				       :ndfa ndfa
				       :label label
				       :initial-p initial-p
				       :final-p final-p)))
	 (when final-p
	   (slot-makunbound ndfa 'final-states)
	   (setf (state-exit-form new-state) exit-form))
	 (when initial-p
	   (slot-makunbound ndfa 'initial-states))
	 (slot-makunbound ndfa 'sticky-states)
	 (dolist (transition transitions)
	   (apply #'add-transition new-state transition))
	 (push new-state (states ndfa))
	 new-state)))))

(defun make-ndfa (state-designators &rest initargs)
  "Factory function for generating an instance of STATE-MACHINE.  STATE-DESIGNATORS is
a list of state-designators.  Each state-designator is a valid initarg list for 
the ADD-STATE function."
  (let ((ndfa (apply #'make-instance 'state-machine :states nil initargs)))
    (dolist (state-designator state-designators)
      (apply #'add-state ndfa state-designator))
    ndfa))

(defun remove-invalid-transitions (dfa &optional (valid-states (states dfa)))
  "Modify all the transition lists for all the states in the DFA, so that any
transition is removed if it leads directly to a state which is not in the list
of valid states. An ERROR is signaled if such removing makes a state
non-coaccessible."
  (dolist (state (states dfa) dfa)
    (setf (transitions state)
	  (setof transition (transitions state)
	    (member (next-state transition) valid-states :test #'eq)))
    (unless (or (transitions state)
		(state-final-p state))
      ;; finals states are the only ones which are allowed to have no transitions
      (error "after removing invalid transitions, a state ~A has become non-coaccessible; this appears to be an internal error"
	     state))))

(defun remove-invalid-states (ndfa valid-states)
  "Remove all states from (states ndfa), (get-final-states ndfa), 
 (get-sticky-states ndfa) and (get-initial-states ndfa)
 which are not in VALID-STATES."
  (declare (type state-machine ndfa)
	   (type list valid-states))

  (setf (states ndfa)
	(intersection (states ndfa) valid-states))

  (slot-makunbound ndfa 'sticky-states)
  (slot-makunbound ndfa 'final-states)
  (slot-makunbound ndfa 'initial-states)
  
  ;; now remove any transitions on remaining states which point
  ;; to one of the states we just finished removing
  (remove-invalid-transitions ndfa valid-states)

  ndfa)

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
  "COMBINE is either nil or a binary function which takes two transition labels and returns a new label representing
 the combination of the two given."
  (declare (type state-machine dfa)
	   (type (or null (function (t t) t)) combine))
  (trim-state-machine dfa)
  (let ((partitions (cons (set-difference (states dfa) (get-final-states dfa))
			  (mapcar #'cadr (group-by (get-final-states dfa) :key #'state-exit-form :test #'equal)))))
    (labels ((find-partition (state)
	       (declare (type state state))
	       (the cons
		    (car (exists partition partitions
			   (member state partition :test #'eq)))))
	     (partition-transition (state)
	       (declare (type state state))
	       (mapcar (lambda (transition)
			 (declare (type transition transition))
			 (list  :with (transition-label transition) :to (find-partition (next-state transition))))
		       (transitions state)))
	     (plist-equal (plist1 plist2)
	       (declare (type (cons keyword cons) plist1 plist2))
	       (and (eq (getf plist1 :to)
			(getf plist2 :to))
		    (funcall equal-labels
			     (getf plist1 :with)
			     (getf plist2 :with))))
	     (refine-partition (partition)
	       ;; partition is a list of states
	       (let ((characterization (group-by partition
						 :key #'partition-transition
						 :test (lambda (plists1 plists2)
							 (not (set-exclusive-or plists1 plists2
										:test #'plist-equal))))))
		 ;; characterization is a car/cadr alist mapping a plist to a list of states which is a subset of partition
		 ;; plist looks like ( :with ...  :to ...)
		 (loop :for grouped-by-transitions :in characterization
		       :collect (destructuring-bind (_plist equiv-states) grouped-by-transitions
				  (declare (ignore _plist))
				  ;; this call to SETOF creates a list with the same elements as equiv-states,
				  ;;   but so that they are order in the same order as they are found
				  ;;   in (STATES DFA).  This is so that FIXED-POINT can depend on the order
				  ;;   and recognize when the same value has been returned twice from REFINE-PARTITIONS.
				  (setof state (states dfa)
				    (member state equiv-states :test #'eq))))))
	     (refine-partitions (p)
	       (setf partitions (mapcan #'refine-partition p))))
	
      (fixed-point #'refine-partitions
		   partitions)

      ;; now build new state machine, and combine parallel transitions using the COMBINE function
      (let* ((reduced-dfa (make-instance (class-of dfa)))
	     (new-state->equiv-class 
	       ;; add new states to reduced-dfa
	       (loop :for equiv-class :in partitions
		     :collect (cons (add-state reduced-dfa
					       :label (state-label (car equiv-class))
					       :initial-p (if (exists state equiv-class
								(state-initial-p state))
							      t nil)
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
   "Given two state machines for which we with to calculate the cross-product,
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
 :FINAL-STATE-CALLBACK -- a callback function to be called each time a new state in the synchronized 
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
	   (type (function (t t) t) match-label))
  (let ((label 0)
	(states->state (make-hash-table :test #'equal))
	(state->states (make-hash-table :test #'eq))
	(buf (list nil)))
    (labels ((calc-final (st1 st2)
	       (declare (type (or null state) st1 st2))
	       (funcall boolean-function (and st1 (state-final-p st1)) (and st2 (state-final-p st2))))
	     (product-state (st1 st2 &key initial-p)
	       (declare (type (or null state) st1 st2))
	       (or (gethash (list st1 st2) states->state nil)
		   (let ((new-state (add-state sm-product
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
	      (when (state-final-p next-product-state)
		(funcall final-state-callback next-product-state next-state-1 next-state-2))
	      (add-transition product-state :next-label (state-label next-product-state) :transition-label transition-label)))))

      (dolist (final-state (get-final-states sm-product))
	(destructuring-bind (st1-from st2-from) (gethash final-state state->states)
	  (funcall final-state-callback final-state st1-from st2-from)))

      (minimize-state-machine sm-product))))

(defgeneric synchronized-product (sm1 sm2 &key boolean-function))

(defmethod synchronized-product ((sm1 state-machine) (sm2 state-machine) &key (boolean-function (lambda (a b) (and a b))))
  (if (eq (class-of sm1)
	  (class-of sm2))
      (populate-synchronized-product (make-instance (class-of sm1)) sm1 sm2 :boolean-function boolean-function)
      (error "Cannot create synchronized product of ~A and ~A" (class-of sm1) (class-of sm2))))
      
