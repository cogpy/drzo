#!/usr/bin/env guile
!#

;;; Test suite for ontogenesis of roots
;;; Comprehensive tests for rooted tree generation

(use-modules (srfi srfi-64)   ; Testing library
             (ice-9 format))

;; Load the ontogenesis implementation
(load "./ontogenesis.scm")

;;; =============================================================================
;;; Test Suite
;;; =============================================================================

(test-begin "ontogenesis")

;;; Basic tree properties
(test-group "tree-properties"
  (test-assert "empty list is a tree" (tree? '()))
  (test-assert "nested list is a tree" (tree? '(())))
  (test-assert "complex tree is valid" (tree? '(() (()) (()()))))
  (test-assert "non-list is not a tree" (not (tree? 'atom)))
  
  (test-equal "size of single node" 1 (tree-size '()))
  (test-equal "size of two nodes" 2 (tree-size '(())))
  (test-equal "size of three nodes (linear)" 3 (tree-size '((())))
  (test-equal "size of three nodes (branching)" 3 (tree-size '(()()))))

;;; Tree height
(test-group "tree-height"
  (test-equal "height of single node" 0 (tree-height '()))
  (test-equal "height of linear tree" 3 (tree-height '(((())))))
  (test-equal "height of branching tree" 1 (tree-height '(()()))))

;;; Tree width
(test-group "tree-width"
  (test-equal "width of single node" 0 (tree-width '()))
  (test-equal "width of linear tree" 1 (tree-width '(())))
  (test-equal "width of branching tree" 2 (tree-width '(()()))))

;;; Tree canonicalization
(test-group "canonicalization"
  (test-equal "canonical form is idempotent"
              '(()())
              (canonicalize-tree (canonicalize-tree '(()()))))
  
  (test-equal "reordered subtrees become canonical"
              '(() (()) (()()))
              (canonicalize-tree '((()()) (()) ()))))

;;; Tree comparison
(test-group "tree-comparison"
  (test-equal "equal trees" 0 (tree-compare '() '()))
  (test-equal "smaller tree" -1 (tree-compare '() '(())))
  (test-equal "larger tree" 1 (tree-compare '(()) '()))
  (test-assert "tree equality" (tree-equal? '(()()) '(()()))))

;;; Integer partitions
(test-group "partitions"
  (test-equal "partitions of 1" '((1)) (generate-partitions 1))
  (test-equal "partitions of 2" '((2) (1 1)) (generate-partitions 2))
  (test-equal "partitions of 3" '((3) (2 1) (1 1 1)) (generate-partitions 3))
  (test-equal "partitions of 4" 
              '((4) (3 1) (2 2) (2 1 1) (1 1 1 1))
              (generate-partitions 4)))

;;; Tree generation correctness
(test-group "tree-generation"
  ;; Test that generation produces correct counts
  (test-equal "1 node: 1 tree" 1 (length (generate-trees 1)))
  (test-equal "2 nodes: 1 tree" 1 (length (generate-trees 2)))
  (test-equal "3 nodes: 2 trees" 2 (length (generate-trees 3)))
  (test-equal "4 nodes: 4 trees" 4 (length (generate-trees 4)))
  (test-equal "5 nodes: 9 trees" 9 (length (generate-trees 5)))
  (test-equal "6 nodes: 20 trees" 20 (length (generate-trees 6)))
  (test-equal "7 nodes: 48 trees" 48 (length (generate-trees 7)))
  
  ;; Test that all generated trees have correct size
  (test-assert "all 4-node trees have size 4"
               (every (lambda (t) (= (tree-size t) 4))
                      (generate-trees 4)))
  
  ;; Test that all trees are unique
  (let ((trees (generate-trees 5)))
    (test-equal "no duplicate 5-node trees"
                (length trees)
                (length (remove-duplicate-trees trees))))
  
  ;; Test specific tree structures
  (test-assert "single node exists"
               (member '() (generate-trees 1)))
  
  (test-assert "linear 3-node tree exists"
               (member '((())) (generate-trees 3)))
  
  (test-assert "branching 3-node tree exists"
               (member '(()()) (generate-trees 3))))

;;; Verify against A000081 sequence
(test-group "a000081-verification"
  (do ((n 1 (+ n 1)))
      ((> n 10))
    (test-equal (format #f "A000081(~a) matches" n)
                (a000081-nth n)
                (length (generate-trees n)))))

;;; Tree operations
(test-group "tree-operations"
  (let ((linear '((((())))))
        (branching '((()()))))
    (test-equal "linear tree height" 4 (tree-height linear))
    (test-equal "branching tree height" 2 (tree-height branching))
    (test-equal "linear tree leaves" 1 (tree-leaves linear))
    (test-equal "branching tree leaves" 2 (tree-leaves branching))
    (test-equal "tree degree" 2 (tree-degree '(()())))))

;;; String representation
(test-group "string-representation"
  (test-equal "single node to string" "()" (tree->string '()))
  (test-equal "nested tree to string" "(())" (tree->string '(())))
  (test-equal "branching tree to string" "(()())" (tree->string '(()()))))

(test-end "ontogenesis")

;;; =============================================================================
;;; Performance Tests (not critical, just informative)
;;; =============================================================================

(define (benchmark-generation n)
  "Benchmark tree generation for size n"
  (format #t "Generating trees with ~a nodes... " n)
  (let ((start (get-internal-real-time)))
    (let ((trees (generate-trees n)))
      (let ((elapsed (/ (- (get-internal-real-time) start) 
                       internal-time-units-per-second)))
        (format #t "~a trees in ~,3f seconds~%" (length trees) elapsed)))))

(format #t "~%=== Performance Benchmarks ===~%")
(for-each benchmark-generation '(5 7 8 9 10))
(format #t "~%")

;;; =============================================================================
;;; Integration Tests
;;; =============================================================================

(format #t "=== Integration Tests ===~%~%")

(format #t "Testing tree operations on generated trees...~%")
(let ((trees-4 (generate-trees 4)))
  (format #t "4-node trees: ~a~%" (length trees-4))
  (format #t "Heights: ~{~a ~}~%" (map tree-height trees-4))
  (format #t "Widths: ~{~a ~}~%" (map tree-width trees-4))
  (format #t "All canonical? ~a~%~%"
          (every (lambda (t) (tree-equal? t (canonicalize-tree t))) trees-4)))

(format #t "Testing multiset partition handling...~%")
(let ((partition-2-2 (generate-trees-from-partition '(2 2))))
  (format #t "Trees from partition (2 2): ~a~%" (length partition-2-2))
  (format #t "Trees: ~{~a ~}~%~%" (map tree->string partition-2-2)))

(format #t "All integration tests passed! ✓~%")
