#!/usr/bin/env guile
!#

;;; Interactive demonstration of Matula numbers
;;; Shows the bijection between rooted trees and positive integers

(use-modules (srfi srfi-1)
             (ice-9 format))

;; Load the main module
(load "a000081.scm")

(define (separator)
  "Print a visual separator"
  (format #t "~%~a~%" (make-string 70 #\=)))

(define (count-nodes tree)
  "Count the total number of nodes in a tree"
  (if (null? tree)
      1  ; Just the root
      (+ 1 (apply + (map count-nodes tree)))))

(define (tree-height tree)
  "Calculate the height of a tree"
  (if (null? tree)
      0
      (+ 1 (apply max 0 (map tree-height tree)))))

(define (tree-to-string tree)
  "Convert tree to a readable string representation"
  (if (null? tree)
      "○"  ; Single vertex
      (format #f "○─[~{~a~^, ~}]" (map tree-to-string tree))))

(define (demo-basic-bijection)
  "Demonstrate basic tree-integer correspondence"
  (separator)
  (format #t "BASIC MATULA BIJECTION~%")
  (separator)
  (format #t "~%Showing correspondence between trees and integers:~%~%")
  
  (format #t "~4@a │ ~25@a │ ~15@a │ ~a~%"
          "Int" "Tree (nested list)" "Prime Factors" "Nodes")
  (format #t "~a~%" (make-string 70 #\─))
  
  (do ((n 1 (+ n 1)))
      ((> n 15))
    (let* ((tree (matula->tree n))
           (factors (prime-factorization n))
           (nodes (count-nodes tree))
           (factor-str (if (= n 1)
                          "1"
                          (string-join
                           (map (lambda (p)
                                  (if (= (cdr p) 1)
                                      (format #f "~a" (car p))
                                      (format #f "~a^~a" (car p) (cdr p))))
                                factors)
                           " × "))))
      (format #t "~4@a │ ~25@a │ ~15@a │ ~a~%"
              n tree factor-str nodes))))

(define (demo-structural-patterns)
  "Show structural patterns in Matula numbers"
  (separator)
  (format #t "STRUCTURAL PATTERNS~%")
  (separator)
  
  (format #t "~%Powers of 2 → Star Trees (root with k single-node children):~%")
  (format #t "~a~%" (make-string 70 #\─))
  (do ((k 1 (+ k 1)))
      ((> k 6))
    (let* ((n (expt 2 k))
           (tree (matula->tree n)))
      (format #t "2^~a = ~3@a → ~a~%" k n tree)))
  
  (format #t "~%First 10 primes → Special tree structures:~%")
  (format #t "~a~%" (make-string 70 #\─))
  (do ((i 1 (+ i 1)))
      ((> i 10))
    (let* ((p (nth-prime i))
           (tree (matula->tree p)))
      (format #t "p_~2@a = ~3@a → ~a~%" i p tree)))
  
  (format #t "~%Perfect squares → Trees with repeated subtrees:~%")
  (format #t "~a~%" (make-string 70 #\─))
  (for-each
   (lambda (n)
     (let ((tree (matula->tree n)))
       (format #t "~2@a² = ~3@a → ~a~%" n (* n n) tree)))
   '(2 3 4 5)))

(define (demo-round-trip)
  "Demonstrate round-trip conversion"
  (separator)
  (format #t "ROUND-TRIP CONVERSION~%")
  (separator)
  (format #t "~%Showing that tree→matula→tree = identity:~%")
  (format #t "(Note: Matula numbers encode unlabeled trees, so child order may differ)~%~%")
  
  (format #t "~25@a │ ~10@a │ ~25@a │ ~a~%"
          "Original Tree" "Matula" "Recovered Tree" "Match?")
  (format #t "~a~%" (make-string 70 #\─))
  
  ;; Note: Using Matula canonical forms (as returned by matula->tree)
  (define test-trees
    (map matula->tree '(1 2 3 4 5 6 7 8 9 16)))
  
  (for-each
   (lambda (tree)
     (let* ((m (tree->matula tree))
            (recovered (matula->tree m))
            (match (equal? tree recovered)))
       (format #t "~25@a │ ~10@a │ ~25@a │ ~a~%"
               tree m recovered (if match "✓" "✗"))))
   test-trees))

(define (demo-tree-properties)
  "Analyze properties of trees by their Matula numbers"
  (separator)
  (format #t "TREE PROPERTIES FROM MATULA NUMBERS~%")
  (separator)
  (format #t "~%Number-theoretic properties reveal tree structure:~%~%")
  
  (format #t "~6@a │ ~15@a │ ~8@a │ ~8@a │ ~20@a~%"
          "Matula" "Type" "Nodes" "Height" "Description")
  (format #t "~a~%" (make-string 70 #\─))
  
  (define (classify-number n)
    (cond
      ((= n 1) "Unit")
      ((is-prime? n) "Prime")
      ((= (length (prime-factorization n)) 1) "Prime power")
      (else "Composite")))
  
  (do ((n 1 (+ n 1)))
      ((> n 20))
    (let* ((tree (matula->tree n))
           (nodes (count-nodes tree))
           (height (tree-height tree))
           (type (classify-number n))
           (desc (cond
                   ((= n 1) "Root only")
                   ((is-prime? n) "Recursive structure")
                   ((= (modulo n 2) 0) (format #f "Has ~a children" 
                                               (length (prime-factorization n))))
                   (else "Mixed branches"))))
      (format #t "~6@a │ ~15@a │ ~8@a │ ~8@a │ ~20@a~%"
              n type nodes height desc))))

(define (demo-tree-visualization)
  "Visualize trees using ASCII art"
  (separator)
  (format #t "TREE VISUALIZATION~%")
  (separator)
  (format #t "~%ASCII representation of trees from Matula numbers:~%~%")
  
  (define (visualize n max-n)
    (when (<= n max-n)
      (let ((tree (matula->tree n)))
        (format #t "Matula ~2@a: ~a~%" n (tree-to-string tree)))
      (visualize (+ n 1) max-n)))
  
  (visualize 1 12))

(define (demo-connection-to-a000081)
  "Show connection between Matula numbers and A000081 sequence"
  (separator)
  (format #t "CONNECTION TO A000081 SEQUENCE~%")
  (separator)
  (format #t "~%A000081 counts rooted trees by number of nodes.~%")
  (format #t "Matula numbers provide an enumeration of these trees.~%~%")
  
  (format #t "~6@a │ ~15@a │ ~40@a~%"
          "Nodes" "Count (A000081)" "Sample Matula Numbers")
  (format #t "~a~%" (make-string 70 #\─))
  
  ;; Find Matula numbers for trees with exactly n nodes
  (define (matula-for-n-nodes n limit)
    "Find Matula numbers for trees with n nodes, up to limit"
    (let loop ((m 1) (found '()))
      (if (> m limit)
          (reverse found)
          (let ((tree (matula->tree m)))
            (if (= (count-nodes tree) n)
                (loop (+ m 1) (cons m found))
                (loop (+ m 1) found))))))
  
  (do ((n 1 (+ n 1)))
      ((> n 8))
    (let* ((count (a000081-nth n))
           (matulas (matula-for-n-nodes n 50))
           (sample (if (> (length matulas) 5)
                      (append (take matulas 5) '(...))
                      matulas)))
      (format #t "~6@a │ ~15@a │ ~{~a~^ ~}~%"
              n count sample))))

(define (main)
  "Run all demonstrations"
  (format #t "~%~%")
  (format #t "╔════════════════════════════════════════════════════════════════════╗~%")
  (format #t "║         MATULA NUMBERS: TREE-INTEGER BIJECTION DEMO               ║~%")
  (format #t "║                                                                    ║~%")
  (format #t "║  Exploring the elegant correspondence between rooted trees         ║~%")
  (format #t "║  and positive integers via prime factorization                     ║~%")
  (format #t "╚════════════════════════════════════════════════════════════════════╝~%")
  
  (demo-basic-bijection)
  (demo-structural-patterns)
  (demo-round-trip)
  (demo-tree-properties)
  (demo-tree-visualization)
  (demo-connection-to-a000081)
  
  (separator)
  (format #t "CONCLUSION~%")
  (separator)
  (format #t "~%The Matula bijection reveals a profound connection:~%")
  (format #t "  • Tree structure ←→ Prime factorization~%")
  (format #t "  • Nested distinctions ←→ Multiplicative structure~%")
  (format #t "  • Rooted trees ←→ Positive integers~%")
  (format #t "~%This unifies combinatorics, number theory, and computation.~%")
  (separator)
  (format #t "~%"))

;; Run the demo
(main)
