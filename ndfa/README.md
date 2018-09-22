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