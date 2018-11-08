# NON-DETERMINISTIC-FINITE-AUTOMATA

## Nickname

NDFA

## Synopsis

Implementation of non-deterministed finite automata


## API

### State Machine API

* `make-ndfa` --  Factory function for generating an instance of `STATE-MACHINE`.  `STATE-DESIGNATORS` is
a list of state-designators.  Each state-designator is a valid initarg list for 
the `ADD-STATE` function.

* `state-machine` -- Class implementing a finite state machine.  An application program is expected to maintain
a list of states, each an element of `(states ...)`, and use either the function
`PERFORM-TRANSITIONS` or `PERFORM-SOME-TRANSITIONS` to compute the list of next states
given an `INPUT-SEQUENCE`.  Use the factory function, `MAKE-NDFA`, to create an instance
of `STATE-MACHINE`. The list of states of the machine may be specified as the
`:STATES` argument to `MAKE-NDFA`, by subsequent calls to `ADD-STATE`.

* `deterministicp` -- Accessor indicating whether a `state-machine` is
determinisitc.  This tells the `perform-some-transitions` function
that it may optimize its algorithm, so that it may follow the first
matching transition rather than all matching transitions.

* `perform-some-transitions` -- Given a list of `STARTING-STATES`, each of which is an element of `(states NDFA)`, 
perform the transitions indicated by `INPUT-SEQUENCE`, i.e., iterate through the
`STARTING-STATES`, and on each element generate a list of next-states, as a function of
the existing transistions on the state.  Some of the transitions will lead to a next state
and some won't; collect a list of such _successful_ next-states.  On this new list of states,
perform the same algorithm on the next element of the `INPUT-SEQUENCE`, and repeat until the
`INPUT-SEQUENCE` is depleted.  Return the list of resulting states.
There is a notable exception, if there is every a transition into a state which is both final
and sticky, then a singleton list of that state is returned, and no further transition, 
nor no further element fo the input-sequence is considered.

* `states` -- Accessor returning the list of all the states of a state machine.

* `add-state` --  Add or update a state designated by the given `LABE`L.  If the state already exists
in the state-machine `NDFA`, (whose `STATE-LABEL` is `EQUAL` to `LABEL`) its `INITIAL-P` and 
`FINAL-P` are updated to `TRUE` if `:INITIAL-P` or `:FINAL-P` are given as such
(but not updated to `NIL`).  If the state does not yet exists, it one is created
and added to the state machine.


* `perform-transitions` -- Returns a list of states which are reached by the following process:
1. Start with the list/set of all initial states of the state-maching NDFA.
2. Iterate through the `INPUT-SEQUENCE`, performing all the applicable transistions.
3. If the list of states becomes empty, return `NIL`.
4. Otherwise the set of states reached is returned.
5. None, some, or all of these states might be final states of the state machine.

* `get-initial-states` -- Return a list of initial states of an NDFA

* `get-final-states` -- Return a list of final states of an NDFA

* `get-sticky-states` -- Return a list of sticky states of an NDFA

* `trim-state-machine` --  Remove all states from the state machine which have no path to a final state or from an initial state.

* `minimize-state-machine` -- Minimize a state machine

* `ndfa-to-dot` --  Render a state machine as `".dot"` or `".png"`.
Calling `NDFA-TO-DOT` with a `PATH` whose type is `"dot"` creates the `".dot"` file, which is input for the
graphviz dot program.   If `PATH` has type `"png"`, a temporary `".dot"` file will be created, and
will be converted to a `".png"` file.

## API for extensibility

* `populate-synchronized-product` -- Given two state machines for which we with to calculate the cross-product,
and a product state machine which has be allocated (as if by make-instance), update the product state machine
with the states comprising the synchronized product.


* `synchronized-product` -- Given two state machines, find their synchronized product by a call to 
`POPULATE-SYNCHRONIZED-PRODUCT`.

### State API

* `state-name` -- Accessor for name of a state

* `state-final-p` -- Accessor to determine whether a state is final

* `state-label` -- Accessor to obtain/retrieve the label of a state

* `state-sticky-p` -- Accessor to determe whether a state is sticky.
A sticky state is one for which all transitions lead immediately
back to the same state.  This allows certain optimizations
expecially in the `perform-some-transitions` function.

* `transitions` -- List of transitions leaving a state.

### Transition API

* `next-label` -- Accessor, label of next state of a transition
* `next-state` -- Accessor, next state of a transition
* `transition-label` -- Accessor, label of a transition.



## Code Example

```lisp
(let ((sm (make-instance 'state-machine :key #'evenp)))
    (add-state sm :label 'a
                  :initial-p t
                  :transitions `((:next-label b :transition-label t)
                                 (:next-label c :transition-label nil)))
    (assert (:states sm))
    (add-state sm :label 'b
                  :final-p t
                  :initial-p t
                  :transitions `((:next-label c :transition-label t)
                                 (:next-label b :transition-label nil)))
    (add-state sm :label 'c
                  :transitions `((:next-label b :transition-label t)
                                 (:next-label c :transition-label nil)))
    (with-output-to-string (str)
      (dolist (state (:states sm))
        (dolist (transition (:transitions state))
          (princ (:next-state transition) str))))
    (mapcar #':transitions (:states sm))
    (assert (:get-initial-states sm))
    (assert (= 2 (length (:get-initial-states sm))))

    (assert (:get-initial-states sm))
    (perform-transitions sm '(1))
    (perform-transitions sm '(1 2))
    (perform-transitions sm #(1 2 3))
    )
```

## License

```
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```