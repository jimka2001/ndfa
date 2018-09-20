## Synopsis

Implementation of non-deterministed finite automata

## Code Examples


### NDFA
```lisp
(let ((sm (make-instance 'ndfa:state-machine :key #'evenp)))
    (ndfa:add-state sm :label 'a
                       :initial-p t
                       :transitions `((:next-label b :transition-label t)
                                      (:next-label c :transition-label nil)))
    (assert (ndfa::states sm))
    (ndfa:add-state sm :label 'b
                       :final-p t
                       :initial-p t
                       :transitions `((:next-label c :transition-label t)
                                      (:next-label b :transition-label nil)))
    (ndfa:add-state sm :label 'c
                       :transitions `((:next-label b :transition-label t)
                                      (:next-label c :transition-label nil)))
    (with-output-to-string (str)
      (dolist (state (ndfa::states sm))
        (dolist (transition (ndfa::transitions state))
          (princ (ndfa::next-state transition) str))))
    (mapcar #'ndfa::transitions (ndfa::states sm))
    (assert (ndfa::get-initial-states sm))
    (assert (= 2 (length (ndfa::get-initial-states sm))))

    (assert (ndfa::get-initial-states sm))
    (ndfa:perform-transitions sm '(1))
    (ndfa:perform-transitions sm '(1 2))
    (ndfa:perform-transitions sm #(1 2 3))
    )
```
