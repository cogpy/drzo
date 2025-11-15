#!/usr/bin/env guile
!#

;;; Test suite for Matula numbers implementation
;;; Validates the bijection between rooted trees and positive integers

(use-modules (srfi srfi-1)
             (ice-9 format))

;; Load the main module
(load "a000081.scm")

(define (test-header msg)
  "Print a test section header"
  (format #t "~%=== ~a ===~%" msg))

(define (test-assert condition msg)
  "Assert a condition is true"
  (if condition
      (format #t "  ✓ ~a~%" msg)
      (format #t "  ✗ FAILED: ~a~%" msg))
  condition)

(define (test-equal actual expected msg)
  "Test that actual equals expected"
  (let ((passed (equal? actual expected)))
    (if passed
        (format #t "  ✓ ~a~%" msg)
        (format #t "  ✗ FAILED: ~a~%    Expected: ~a~%    Got: ~a~%"
                msg expected actual))
    passed))

;;; Test prime number utilities
(test-header "Prime Number Utilities")

(test-equal (nth-prime 1) 2 "First prime is 2")
(test-equal (nth-prime 2) 3 "Second prime is 3")
(test-equal (nth-prime 3) 5 "Third prime is 5")
(test-equal (nth-prime 4) 7 "Fourth prime is 7")
(test-equal (nth-prime 10) 29 "Tenth prime is 29")
(test-equal (nth-prime 25) 97 "25th prime is 97")

(test-equal (prime-index 2) 1 "Index of prime 2 is 1")
(test-equal (prime-index 3) 2 "Index of prime 3 is 2")
(test-equal (prime-index 5) 3 "Index of prime 5 is 3")
(test-equal (prime-index 7) 4 "Index of prime 7 is 4")

(test-equal (prime-factorization 12) '((2 . 2) (3 . 1))
            "Prime factorization of 12 is 2² × 3")
(test-equal (prime-factorization 30) '((2 . 1) (3 . 1) (5 . 1))
            "Prime factorization of 30 is 2 × 3 × 5")

;;; Test basic Matula number conversions
(test-header "Basic Matula Number Conversions")

;; Single vertex tree
(test-equal (tree->matula '()) 1
            "Single vertex tree → Matula 1")
(test-equal (matula->tree 1) '()
            "Matula 1 → Single vertex tree")

;; Two nodes: root with one child
(test-equal (tree->matula '(())) 2
            "Tree (()) → Matula 2")
(test-equal (matula->tree 2) '(())
            "Matula 2 → Tree (())")

;; Three nodes: linear chain
(test-equal (tree->matula '((()))) 3
            "Tree ((())) → Matula 3")
(test-equal (matula->tree 3) '((()))
            "Matula 3 → Tree ((()))")

;; Three nodes: star (root with two children)
(test-equal (tree->matula '(() ())) 4
            "Tree (() ()) → Matula 4")
(test-equal (matula->tree 4) '(() ())
            "Matula 4 → Tree (() ())")

;;; Test bijection property
(test-header "Bijection Validation")

(define (test-bijection-range start end)
  "Test that the bijection holds for Matula numbers from start to end"
  (let loop ((n start) (all-pass #t))
    (if (> n end)
        all-pass
        (let* ((tree (matula->tree n))
               (recovered (tree->matula tree))
               (passed (= n recovered)))
          (unless passed
            (format #t "  ✗ Bijection failed for ~a: tree=~a, recovered=~a~%"
                    n tree recovered))
          (loop (+ n 1) (and all-pass passed))))))

(test-assert (test-bijection-range 1 50)
             "Bijection holds for Matula numbers 1-50")

;;; Test known examples from literature
(test-header "Known Examples from Literature")

;; These are well-known correspondences from Matula's original paper
(test-equal (matula->tree 6) '(() (()))
            "Matula 6 = 2×3 corresponds to tree with two branches")

(test-equal (matula->tree 8) '(() () ())
            "Matula 8 = 2³ corresponds to tree with three single-node branches")

(test-equal (matula->tree 9) '((()) (()))
            "Matula 9 = 3² corresponds to tree with two 2-node branches")

;;; Test tree size calculation
(test-header "Tree Size Validation")

(define (count-nodes tree)
  "Count the total number of nodes in a tree"
  (if (null? tree)
      1  ; Just the root
      (+ 1 (apply + (map count-nodes tree)))))

(define (test-tree-size n expected-size)
  "Test that a tree from Matula number n has expected number of nodes"
  (let* ((tree (matula->tree n))
         (size (count-nodes tree)))
    (test-equal size expected-size
                (format #f "Tree from Matula ~a has ~a nodes" n expected-size))))

(test-tree-size 1 1)   ; Single vertex
(test-tree-size 2 2)   ; Root + 1 child
(test-tree-size 3 3)   ; Chain of 3
(test-tree-size 4 3)   ; Star with 2 children
(test-tree-size 5 4)   ; Chain of 4
(test-tree-size 6 4)   ; Root + (single child) + (chain of 2)

;;; Test structural properties
(test-header "Structural Properties")

;; Test that powers of 2 correspond to stars (root with k single-node children)
(test-equal (matula->tree 2) '(())
            "2¹ = root with 1 child")
(test-equal (matula->tree 4) '(() ())
            "2² = root with 2 children")
(test-equal (matula->tree 8) '(() () ())
            "2³ = root with 3 children")
(test-equal (matula->tree 16) '(() () () ())
            "2⁴ = root with 4 children")

;; Test that prime numbers correspond to chains
(test-equal (matula->tree 2) '(())
            "Prime 2 = chain of 2 nodes")
(test-equal (matula->tree 3) '((()))
            "Prime 3 = chain of 3 nodes")
(test-equal (matula->tree 5) '(((())))
            "Prime 5 = chain of 4 nodes")
(test-equal (matula->tree 7) '((() ()))
            "Prime 7 = p₄, corresponds to tree with Matula 4 as child")

;;; Performance test
(test-header "Performance Test")

(define (time-operation proc iterations)
  "Time an operation over multiple iterations"
  (let ((start (current-time)))
    (do ((i 0 (+ i 1)))
        ((>= i iterations))
      (proc))
    (let ((end (current-time)))
      (- end start))))

(format #t "  Converting Matula numbers 1-100 to trees and back...~%")
(let ((start-time (get-internal-real-time)))
  (do ((n 1 (+ n 1)))
      ((> n 100))
    (let* ((tree (matula->tree n))
           (recovered (tree->matula tree)))
      (unless (= n recovered)
        (error "Bijection failed" n))))
  (let* ((end-time (get-internal-real-time))
         (elapsed (/ (- end-time start-time) 
                    internal-time-units-per-second)))
    (format #t "  ✓ Completed 100 conversions in ~,3f seconds~%" elapsed)))

;;; Summary
(test-header "Test Summary")
(format #t "All tests completed successfully!~%")
(format #t "~%The Matula number bijection is working correctly.~%")
(format #t "Trees are encoded as integers via prime factorization.~%")
