#!/usr/bin/env guile
!#

;;; A000081 - Number of unlabeled rooted trees with n nodes
;;; Ghost in the Guile Shell
;;;
;;; This module implements the A000081 sequence and related mathematical formulas
;;; as specified in the problem statement.

(use-modules (srfi srfi-1)
             (ice-9 format))

;;; Core sequence definition: {0,1,1,2,4,9,20,48,115,286,719,...}
(define *a000081-cache* (make-vector 1000 #f))

;;; Initialize the first few known values
(vector-set! *a000081-cache* 0 0)
(vector-set! *a000081-cache* 1 1)

;;; Compute divisors of n
(define (compute-divisors n)
  "Return list of all positive divisors of n"
  (if (<= n 1)
      (list 1)
      (let loop ((d 1) (divisors '()))
        (cond
          ((> d n) (reverse divisors))
          ((= (modulo n d) 0) (loop (+ d 1) (cons d divisors)))
          (else (loop (+ d 1) divisors))))))

;;; Sum of divisors weighted by a_d
(define (sum-of-divisors k a-fn)
  "Compute ∑_{d|k} d·a_d"
  (let ((divisors (compute-divisors k)))
    (apply + (map (lambda (d) (* d (a-fn d))) divisors))))

;;; The main recursive formula:
;;; ∀ n ∈ N⁺, a_{n+1} = (1/n)∑_{k=1}^n(∑_{d|k}d·a_d)a_{n-k+1}
(define (a000081-recursive n)
  "Compute A000081(n) using the recursive formula"
  (cond
    ((< n 0) 0)
    ((= n 0) 0)
    ((= n 1) 1)
    ((vector-ref *a000081-cache* n) 
     (vector-ref *a000081-cache* n))
    (else
     (let* ((result
             (let loop ((k 1) (sum 0))
               (if (> k (- n 1))
                   (/ sum (- n 1))
                   (let* ((divisor-sum (sum-of-divisors k a000081-recursive))
                          (a-term (a000081-recursive (- n k)))
                          (term (* divisor-sum a-term)))
                     (loop (+ k 1) (+ sum term)))))))
       (vector-set! *a000081-cache* n (inexact->exact (round result)))
       (vector-ref *a000081-cache* n)))))

;;; Get the nth term of A000081
(define (a000081-nth n)
  "Get the nth term of the A000081 sequence"
  (a000081-recursive n))

;;; Generate sequence up to n terms
(define (a000081-sequence n)
  "Generate the first n terms of A000081 sequence"
  (map a000081-nth (iota n)))

;;; Asymptotic approximation: a_n ~ C·α^n·n^{-3/2}
;;; where α ≈ 2.9557652857...
(define *alpha* 2.9557652857)
(define *C* 0.4399237) ; Computed constant

(define (asymptotic-approximation n)
  "Asymptotic approximation: a_n ~ C·α^n·n^{-3/2}"
  (if (<= n 0)
      0
      (* *C* (expt *alpha* n) (expt n -1.5))))

;;; Display utilities
(define (display-sequence n)
  "Display the first n terms of A000081"
  (format #t "A000081 sequence (first ~a terms):~%" n)
  (let ((seq (a000081-sequence n)))
    (format #t "~{~a~^, ~}~%" seq)
    (format #t "~%")))

;;; Generating function related computations
(define (generating-function-coeffs x terms)
  "Compute coefficients for the generating function A(x) = ∑ a_n x^n"
  (let ((seq (a000081-sequence terms)))
    (apply + (map (lambda (n an) (* an (expt x n))) 
                  (iota terms) seq))))

;;; ============================================================================
;;; Matula Numbers - Bijection between rooted trees and positive integers
;;; ============================================================================
;;;
;;; Matula numbers encode rooted trees as integers via prime factorization.
;;; 
;;; The bijection works as follows:
;;; - The single-vertex tree (root only) has Matula number 1
;;; - For a tree T with root having subtrees T₁, T₂, ..., Tₖ with Matula 
;;;   numbers m₁, m₂, ..., mₖ, the Matula number is: ∏ᵢ p_{mᵢ}
;;;   where p_j is the j-th prime number
;;;
;;; Examples:
;;;   Tree ()           → Matula number 1 (single vertex)
;;;   Tree (())         → Matula number 2 (p₁ = 2)
;;;   Tree ((()))       → Matula number 3 (p₂ = 3, since subtree is 2)
;;;   Tree (()())       → Matula number 4 (2 × 2 = 4, two subtrees each with number 1)
;;;
;;; Reference: D.W. Matula, "A natural rooted tree enumeration by prime 
;;; factorization", SIAM Rev. 10 (1968) 273.

;;; Prime number utilities
(define *prime-cache* (make-vector 1000 #f))

(define (is-prime? n)
  "Check if n is a prime number"
  (cond
    ((<= n 1) #f)
    ((= n 2) #t)
    ((= (modulo n 2) 0) #f)
    (else
     (let loop ((d 3))
       (cond
         ((> (* d d) n) #t)
         ((= (modulo n d) 0) #f)
         (else (loop (+ d 2))))))))

(define (nth-prime n)
  "Get the nth prime number (1-indexed: p₁=2, p₂=3, p₃=5, ...)"
  (cond
    ((< n 1) (error "Prime index must be positive"))
    ((vector-ref *prime-cache* n)
     (vector-ref *prime-cache* n))
    (else
     (let loop ((candidate 2) (count 0))
       (if (is-prime? candidate)
           (if (= (+ count 1) n)
               (begin
                 (vector-set! *prime-cache* n candidate)
                 candidate)
               (loop (+ candidate 1) (+ count 1)))
           (loop (+ candidate 1) count))))))

(define (prime-factorization n)
  "Return the prime factorization of n as a list of (prime . exponent) pairs"
  (if (<= n 1)
      '()
      (let loop ((n n) (factor 2) (factors '()))
        (cond
          ((= n 1) (reverse factors))
          ((> (* factor factor) n)
           (reverse (cons (cons n 1) factors)))
          ((= (modulo n factor) 0)
           (let count-loop ((n n) (exp 0))
             (if (= (modulo n factor) 0)
                 (count-loop (/ n factor) (+ exp 1))
                 (loop n (+ factor 1) (cons (cons factor exp) factors)))))
          (else (loop n (+ factor 1) factors))))))

(define (prime-index p)
  "Get the index of prime p (inverse of nth-prime)"
  (let loop ((n 1))
    (let ((pn (nth-prime n)))
      (cond
        ((= pn p) n)
        ((> pn p) (error "Not a prime number" p))
        (else (loop (+ n 1)))))))

;;; Tree representation
;;; We represent rooted trees as nested lists:
;;; - '() represents the single-vertex tree (just the root)
;;; - '(T₁ T₂ ... Tₖ) represents a tree with root having k subtrees

(define (tree->matula tree)
  "Convert a rooted tree (as nested list) to its Matula number"
  (if (null? tree)
      1  ; Single vertex tree
      (apply * (map (lambda (subtree)
                      (nth-prime (tree->matula subtree)))
                    tree))))

(define (matula->tree m)
  "Convert a Matula number to its corresponding rooted tree"
  (if (= m 1)
      '()  ; Single vertex tree
      (let ((factors (prime-factorization m)))
        (apply append
               (map (lambda (factor-pair)
                      (let ((prime (car factor-pair))
                            (exp (cdr factor-pair)))
                        (let ((subtree-matula (prime-index prime)))
                          (make-list exp (matula->tree subtree-matula)))))
                    factors)))))

(define (validate-matula-bijection n)
  "Validate that tree->matula and matula->tree are inverse functions for Matula number n"
  (let ((tree (matula->tree n)))
    (let ((recovered (tree->matula tree)))
      (if (= n recovered)
          #t
          (begin
            (format #t "Validation failed for ~a: tree=~a, recovered=~a~%"
                    n tree recovered)
            #f)))))

;;; Enumerate all Matula numbers for trees with n nodes
(define (matula-numbers-for-n-nodes n)
  "Generate all Matula numbers corresponding to rooted trees with n nodes"
  (if (<= n 0)
      '()
      (let enumerate-trees ((n n))
        (cond
          ((= n 1) '(() )) ; Single vertex tree
          (else
           ;; Generate all rooted trees with n nodes by considering
           ;; all partitions of (n-1) among subtrees
           (let loop ((k 1) (all-trees '()))
             (if (>= k n)
                 all-trees
                 (let* ((smaller-trees (enumerate-trees k))
                        (remaining (- n 1 k))
                        (rest-trees (enumerate-trees remaining)))
                   (let combine ((st smaller-trees) (new-trees all-trees))
                     (if (null? st)
                         (loop (+ k 1) new-trees)
                         (let add-rest ((rt rest-trees) (newer-trees new-trees))
                           (if (null? rt)
                               (combine (cdr st) newer-trees)
                               (add-rest (cdr rt)
                                        (cons (cons (car st) (car rt))
                                              newer-trees))))))))))))))

(define (display-matula-examples n)
  "Display Matula number examples for trees up to n nodes"
  (format #t "=== Matula Numbers - Tree/Integer Bijection ===~%~%")
  (format #t "Matula numbers for small trees:~%")
  (do ((i 1 (+ i 1)))
      ((> i n))
    (let* ((tree (if (= i 1) '() 
                     (case i
                       ((2) '(()))           ; Tree with 2 nodes: root with 1 child
                       ((3) '((()) ()))      ; Tree with 3 nodes: root with 2 children
                       ((4) '((())))         ; Tree with 4 nodes: linear chain
                       (else '()))))
           (matula (tree->matula tree)))
      (when (not (null? tree))
        (format #t "  Tree ~a → Matula number ~a~%" tree matula))))
  (format #t "~%")
  
  ;; Show reconstruction
  (format #t "Reconstructing trees from Matula numbers:~%")
  (do ((m 1 (+ m 1)))
      ((> m 10))
    (let ((tree (matula->tree m)))
      (format #t "  Matula ~2d → Tree ~a~%" m tree)))
  (format #t "~%")
  
  ;; Validate bijection
  (format #t "Validating bijection (first 20 Matula numbers):~%")
  (let ((all-valid #t))
    (do ((m 1 (+ m 1)))
        ((> m 20))
      (unless (validate-matula-bijection m)
        (set! all-valid #f)))
    (if all-valid
        (format #t "  ✓ All validations passed!~%")
        (format #t "  ✗ Some validations failed~%")))
  (format #t "~%"))

;;; Main demonstration
(define (main)
  "Main demonstration of A000081 computations"
  (format #t "=== Ghost in the Guile Shell ===~%")
  (format #t "A000081: Number of unlabeled rooted trees~%~%")
  
  ;; Display the sequence
  (display-sequence 15)
  
  ;; Show recursive computation for specific terms
  (format #t "Specific computations:~%")
  (do ((i 1 (+ i 1)))
      ((> i 10))
    (format #t "a(~a) = ~a~%" i (a000081-nth i)))
  (format #t "~%")
  
  ;; Asymptotic comparison
  (format #t "Asymptotic approximation comparison:~%")
  (format #t "n\tExact\tAsymptotic\tRatio~%")
  (do ((i 5 (+ i 1)))
      ((> i 12))
    (let ((exact (a000081-nth i))
          (approx (asymptotic-approximation i)))
      (format #t "~a\t~a\t~,3f\t\t~,4f~%" 
              i exact approx (/ approx exact))))
  (format #t "~%")
  
  ;; Show generating function evaluation
  (format #t "Generating function A(0.1) ≈ ~,6f~%" 
          (generating-function-coeffs 0.1 20))
  (format #t "Generating function A(0.2) ≈ ~,6f~%" 
          (generating-function-coeffs 0.2 20))
  (format #t "~%")
  
  ;; Demonstrate Matula numbers
  (display-matula-examples 4))

;; If this file is run directly
(when (and (defined? 'command-line) (not (null? (command-line))))
  (main))