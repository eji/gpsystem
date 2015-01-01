#!/usr/bin/env gosh

(use gauche.test)

(test-start "gpsystem util tests")

(require "gpsystem/util")
(import gpsystem.util)
(test-module 'gpsystem.util)


(test-section "func:init")
(test* "null is not accepted" (test-error <error>) (init '()))
(test* "return a list without last item" '() (init '(a)))
(test* "return a list without last item" '(a) (init '(a b)))
(test* "return a list without last item" '(a b) (init '(a b c)))


(test-section "func:take")
(test* "return null if zero passed" '() (take 0 '(a b c)))
(test* "return null if minus number passed" '() (take -1 '(a b c)))
(test* "return a list including all items if passed number is longger than passed list length"
       '(a b c) (take 4 '(a b c)))
(test* "return a list including items"
       '(a b) (take 2 '(a b c)))


(test-section "func:shuffle")
(test* "return null if null passed" '() (shuffle '()))
(test* "return shuffled list" '(a) (shuffle '(a)))
(test* "return shuffled list"
       (test-one-of
         '(a b c)
         '(a c b)
         '(b a c)
         '(b c a)
         '(c a b)
         '(c b a))
       (shuffle '(a b c)))


(test-section "func:random-find")
(test* "return #f if no matched item" #f (random-find even? '(1 3 5)))
(test* "return a random selected item"
       (test-one-of 2 4 6 8 10)
       (random-find even? '(1 2 3 4 5 6 7 8 9 10)))


(test-section "func:replace")
(test* "return a same list if no item matched"
       '(a b c d)
       (replace 'e 'replaced '(a b c d)))
(test* "return a list matched item replaced"
       '(a replaced c replaced d) 
       (replace 'b 'replaced '(a b c b d)))


(test-section "func:clone")

(define-class <clone-test-class> ()
  ((fld1 :init-keyword :fld1)))
(define base-obj (make <clone-test-class> :fld1 "fld1"))
(define cloned-obj (clone base-obj))

(test* "cloned obj is not eq obj" #f (eq? cloned-obj base-obj))
(test* "cloned obj's slots has eq obj"
       #t
       (eq? (slot-ref cloned-obj 'fld1) (slot-ref base-obj 'fld1)))

(test-end :exit-on-failure #t)
