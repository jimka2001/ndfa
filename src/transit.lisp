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

(defun find-transit (sm)
  (declare (type state-machine sm))
  "Given a STATE-MACHINE finds a path from an initial state to a final state, if such exists.
 The return value indicates a list of types of the elements of a sequence which would take the
 state machine through the states of that path.
 Returns two values:  the second return value is T if a transit is found, NIL otherwise.
   If a transit is found, then the first return value is the type signature of such a transit."
  (labels ((extract (transition-history)
             (return-from find-transit (values (nreverse (mapcar #'transition-label transition-history)) t)))
           (transit (state transition-history)
               (cond
                 ((state-final-p state)
                  (extract transition-history))
                 (t
                  (dolist (transition (transitions state))
                    (unless (member transition transition-history :test #'eq)
                      (transit (next-state transition) (cons transition transition-history))))))))
    (dolist (state (get-initial-states sm))
      (transit state nil))
    (values nil nil)))
