#!/usr/bin/env guile
!#

;;; Ontogenesis of Roots - Rooted Tree Generation
;;; Implementation of the bag chain algorithm for generating rooted trees
;;;
;;; This module implements the actual generation of rooted tree structures
;;; as recursive distinctions following Spencer-Brown's Laws of Form.
;;;
;;; A rooted tree is represented as a nested list (S-expression):
;;;   ()           - 1-node tree (single distinction)
;;;   (())         - 2-node tree (nested distinction)
;;;   (()())       - 3-node tree (branching distinction)
;;;   ((()))       - 3-node tree (linear distinction)

(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 format))

;; Load core A000081 implementation for verification
(load "./a000081.scm")

;;; =============================================================================
;;; Tree Representation
;;; =============================================================================

;;; A rooted tree is represented as a list of subtrees
;;; The empty list () represents a single node (the root with no children)
;;; A list like (()) represents a root with one child (which has no children)
;;; A list like (()()) represents a root with two children (both childless)

(define (tree? obj)
  "Check if obj is a valid rooted tree (list of trees)"
  (or (null? obj)  ; Base case: single node
      (and (list? obj)
           (every tree? obj))))  ; Recursive case: list of trees

(define (tree-size tree)
  "Count the number of nodes in a tree (including root)"
  (if (null? tree)
      1  ; Single node
      (+ 1 (apply + (map tree-size tree)))))  ; Root + sum of subtrees

(define (tree-equal? t1 t2)
  "Check if two trees are structurally equal"
  (equal? t1 t2))

;;; =============================================================================
;;; Tree Canonicalization
;;; =============================================================================

;;; Trees need to be in canonical form to avoid duplicates
;;; Canonical form: subtrees are sorted in a consistent order

(define (tree-compare t1 t2)
  "Compare two trees lexicographically for sorting
   Returns: -1 if t1 < t2, 0 if t1 = t2, 1 if t1 > t2"
  (cond
    ((and (null? t1) (null? t2)) 0)
    ((null? t1) -1)
    ((null? t2) 1)
    ((< (length t1) (length t2)) -1)
    ((> (length t1) (length t2)) 1)
    (else
     ;; Same length, compare element by element
     (let loop ((l1 t1) (l2 t2))
       (if (null? l1)
           0
           (let ((cmp (tree-compare (car l1) (car l2))))
             (if (= cmp 0)
                 (loop (cdr l1) (cdr l2))
                 cmp)))))))

(define (tree-less? t1 t2)
  "Check if t1 < t2 in canonical ordering"
  (< (tree-compare t1 t2) 0))

(define (canonicalize-tree tree)
  "Put a tree in canonical form (sorted subtrees)"
  (if (null? tree)
      tree
      (sort (map canonicalize-tree tree) tree-less?)))

;;; =============================================================================
;;; Tree Generation: Ontogenesis
;;; =============================================================================

;;; Generate all rooted trees with n nodes
;;; This is the core "ontogenesis" - the process by which trees come into being

(define *tree-cache* (make-hash-table))

(define (generate-trees n)
  "Generate all non-isomorphic rooted trees with n nodes
   Returns a list of trees in canonical form"
  (cond
    ((< n 1) '())
    ((= n 1) '(()))  ; Single tree with 1 node: ()
    (else
     (or (hash-ref *tree-cache* n)
         (let ((trees (generate-trees-uncached n)))
           (hash-set! *tree-cache* n trees)
           trees)))))

(define (generate-trees-uncached n)
  "Generate trees without caching (internal use)"
  ;; To make a tree with n nodes:
  ;; 1. Take n-1 nodes for the children (forest)
  ;; 2. Partition these n-1 nodes into k groups
  ;; 3. Generate all trees for each group
  ;; 4. Combine them as children of the root
  (let ((m (- n 1)))  ; nodes available for children
    (if (= m 0)
        '(())  ; Just the root
        (apply append
               (map (lambda (partition)
                      (generate-trees-from-partition partition))
                    (generate-partitions m))))))

;;; =============================================================================
;;; Integer Partitions
;;; =============================================================================

(define (generate-partitions n)
  "Generate all integer partitions of n
   Returns list of partitions as lists, in non-increasing order
   Example: (generate-partitions 4) => ((4) (3 1) (2 2) (2 1 1) (1 1 1 1))"
  (generate-partitions-helper n n))

(define (generate-partitions-helper n max-part)
  "Generate partitions of n with parts <= max-part"
  (cond
    ((<= n 0) '(()))
    ((<= max-part 0) '())
    ((= n 1) '((1)))
    (else
     ;; Either use max-part or don't
     (append
      ;; Use max-part
      (if (<= max-part n)
          (map (lambda (rest) (cons max-part rest))
               (generate-partitions-helper (- n max-part) max-part))
          '())
      ;; Don't use max-part, try smaller
      (generate-partitions-helper n (- max-part 1))))))

;;; =============================================================================
;;; Tree Generation from Partition
;;; =============================================================================

(define (generate-trees-from-partition partition)
  "Generate all trees that can be formed by combining subtrees
   according to the given partition
   
   Example: partition (2 1) means one subtree with 2 nodes 
   and one with 1 node
   
   Need to handle multisets properly - if partition has repeated values,
   we must avoid generating duplicate trees"
  (let ((grouped (group-partition partition)))
    (remove-duplicate-trees
     (map canonicalize-tree
          (generate-multiset-combinations grouped)))))

(define (group-partition partition)
  "Group partition into (size . count) pairs
   Example: (2 2 1) => ((2 . 2) (1 . 1))"
  (if (null? partition)
      '()
      (let loop ((p partition) (current (car partition)) (count 1) (result '()))
        (cond
          ((null? (cdr p))
           (reverse (cons (cons current count) result)))
          ((= (cadr p) current)
           (loop (cdr p) current (+ count 1) result))
          (else
           (loop (cdr p) (cadr p) 1 (cons (cons current count) result)))))))

(define (generate-multiset-combinations grouped)
  "Generate all combinations respecting multiplicities
   grouped is a list of (size . count) pairs"
  (if (null? grouped)
      '(())
      (let* ((first-group (car grouped))
             (size (car first-group))
             (count (cdr first-group))
             (rest-combos (generate-multiset-combinations (cdr grouped)))
             (trees-of-size (generate-trees size)))
        ;; For each way to choose 'count' trees from trees-of-size,
        ;; combine with rest
        (apply append
               (map (lambda (chosen-trees)
                      (map (lambda (rest-combo)
                             (append chosen-trees rest-combo))
                           rest-combos))
                    (combinations-with-replacement trees-of-size count))))))

(define (combinations-with-replacement items k)
  "Generate all k-combinations of items with replacement
   Returns multisets as lists"
  (cond
    ((= k 0) '(()))
    ((null? items) '())
    (else
     (append
      ;; Include first item
      (map (lambda (rest) (cons (car items) rest))
           (combinations-with-replacement items (- k 1)))
      ;; Don't include first item (try others)
      (combinations-with-replacement (cdr items) k)))))

;;; Remove duplicates from tree list
(define (remove-duplicate-trees trees)
  "Remove duplicate trees from list"
  (let loop ((trees trees) (seen '()))
    (if (null? trees)
        (reverse seen)
        (let ((tree (car trees)))
          (if (any (lambda (t) (tree-equal? t tree)) seen)
              (loop (cdr trees) seen)
              (loop (cdr trees) (cons tree seen)))))))

;;; =============================================================================
;;; Tree Operations
;;; =============================================================================

(define (tree-height tree)
  "Calculate the height of a tree (longest path from root to leaf)"
  (if (null? tree)
      0
      (+ 1 (apply max (cons 0 (map tree-height tree))))))

(define (tree-width tree)
  "Calculate the width of a tree (number of immediate children)"
  (length tree))

(define (tree-leaves tree)
  "Count the number of leaves in a tree"
  (if (null? tree)
      1  ; A single node is a leaf
      (apply + (map tree-leaves tree))))

(define (tree-internal-nodes tree)
  "Count internal (non-leaf) nodes"
  (- (tree-size tree) (tree-leaves tree)))

(define (tree-degree tree)
  "Maximum degree (number of children) in the tree"
  (if (null? tree)
      0
      (max (length tree)
           (apply max (cons 0 (map tree-degree tree))))))

;;; =============================================================================
;;; Display and Formatting
;;; =============================================================================

(define (tree->string tree)
  "Convert tree to string representation"
  (if (null? tree)
      "()"
      (format #f "(~{~a~})" (map tree->string tree))))

(define (tree->lisp-form tree)
  "Convert tree to readable Lisp-like form with labels"
  (define (helper tree depth)
    (if (null? tree)
        "leaf"
        (format #f "(node~{ ~a~})" 
                (map (lambda (t) (helper t (+ depth 1))) tree))))
  (helper tree 0))

(define (display-tree tree)
  "Display a single tree"
  (format #t "~a~%" (tree->string tree)))

(define (display-tree-detailed tree)
  "Display tree with statistics"
  (format #t "Tree: ~a~%" (tree->string tree))
  (format #t "  Size: ~a  Height: ~a  Width: ~a  Leaves: ~a  Degree: ~a~%"
          (tree-size tree)
          (tree-height tree)
          (tree-width tree)
          (tree-leaves tree)
          (tree-degree tree)))

(define (display-trees trees)
  "Display all trees in a list"
  (for-each display-tree trees))

(define (display-trees-for-n n)
  "Display all rooted trees with n nodes"
  (format #t "=== All rooted trees with ~a node~a ===~%" 
          n (if (= n 1) "" "s"))
  (let ((trees (generate-trees n)))
    (format #t "Count: ~a (Expected: ~a)~%~%" 
            (length trees) (a000081-nth n))
    (display-trees trees)
    (format #t "~%")))

;;; =============================================================================
;;; Verification
;;; =============================================================================

(define (verify-tree-generation max-n)
  "Verify that tree generation matches A000081 sequence"
  (format #t "=== Verifying Tree Generation ===~%~%")
  (format #t "n\tGenerated\tExpected\tMatch?~%")
  (format #t "─\t─────────\t────────\t──────~%")
  (let loop ((n 1))
    (when (<= n max-n)
      (let* ((trees (generate-trees n))
             (count (length trees))
             (expected (a000081-nth n))
             (match? (= count expected)))
        (format #t "~a\t~a\t\t~a\t\t~a~%" 
                n count expected (if match? "✓" "✗"))
        (loop (+ n 1)))))
  (format #t "~%"))

;;; =============================================================================
;;; Examples and Demonstrations
;;; =============================================================================

(define (demonstrate-small-trees)
  "Show small trees with details"
  (format #t "=== Detailed Tree Analysis ===~%~%")
  (for-each 
   (lambda (n)
     (format #t "Trees with ~a node~a:~%" n (if (= n 1) "" "s"))
     (for-each display-tree-detailed (generate-trees n))
     (format #t "~%"))
   (iota 5 1)))

(define (demonstrate-ontogenesis)
  "Demonstrate the ontogenesis of rooted trees"
  (format #t "╔═══════════════════════════════════════════════════════════════╗~%")
  (format #t "║                  ONTOGENESIS OF ROOTS                         ║~%")
  (format #t "║         Rooted Trees as Recursive Distinctions                ║~%")
  (format #t "╚═══════════════════════════════════════════════════════════════╝~%~%")
  
  (format #t "=== Spencer-Brown's Laws of Form: The Mark of Distinction ===~%~%")
  (format #t "Each tree is a hierarchical system of nested distinctions.~%")
  (format #t "Parentheses represent boundaries between marked/unmarked space.~%~%")
  
  ;; Show small examples
  (for-each display-trees-for-n (iota 6 1))
  
  ;; Verification
  (verify-tree-generation 10)
  
  ;; Show growth
  (format #t "=== Growth of Structural Complexity ===~%~%")
  (format #t "n\tCount\tStructural Patterns~%")
  (format #t "─\t─────\t──────────────────~%")
  (do ((n 1 (+ n 1)))
      ((> n 12))
    (let ((count (a000081-nth n)))
      (format #t "~a\t~a~%" n count)))
  (format #t "~%")
  
  ;; Statistics
  (format #t "=== Tree Statistics Analysis ===~%~%")
  (demonstrate-tree-statistics 6)
  
  ;; Philosophical reflection
  (format #t "=== Philosophical Implications ===~%~%")
  (format #t "The enumeration reveals the explosion of organizational~%")
  (format #t "complexity as we add more distinctions (nodes).~%~%")
  (format #t "Each tree represents a unique way of recursively~%")
  (format #t "subdividing space through nested containment.~%~%")
  (format #t "This is the ontogenesis - the coming-into-being of~%")
  (format #t "structural forms from pure distinction. 🌲✨~%~%")
  
  ;; Lisp connection
  (format #t "=== Connection to Lisp ===~%~%")
  (format #t "Every rooted tree is an S-expression:~%")
  (format #t "  ()       -> NIL or empty list~%")
  (format #t "  (())     -> List containing empty list~%")
  (format #t "  (()())   -> List with two empty lists~%~%")
  (format #t "This is the foundation of homoiconicity:~%")
  (format #t "Code and data share the same recursive structure! 🔄~%"))

(define (demonstrate-tree-statistics n)
  "Show statistics for trees of size n"
  (format #t "Analyzing trees with ~a nodes:~%~%" n)
  (let ((trees (generate-trees n)))
    (format #t "Total count: ~a~%~%" (length trees))
    (format #t "Height distribution:~%")
    (let ((heights (map tree-height trees)))
      (for-each (lambda (h)
                  (format #t "  Height ~a: ~a trees~%"
                          h (count (lambda (x) (= x h)) heights)))
                (iota (+ 1 (apply max heights)) 1)))
    (format #t "~%Width distribution:~%")
    (let ((widths (map tree-width trees)))
      (for-each (lambda (w)
                  (format #t "  Width ~a: ~a trees~%"
                          w (count (lambda (x) (= x w)) widths)))
                (iota (+ 1 (apply max widths)) 0)))
    (format #t "~%")))

;;; =============================================================================
;;; Interactive Utilities
;;; =============================================================================

(define (explore-tree n index)
  "Explore a specific tree from the enumeration"
  (let ((trees (generate-trees n)))
    (if (or (< index 0) (>= index (length trees)))
        (format #t "Index ~a out of range [0, ~a)~%" index (length trees))
        (let ((tree (list-ref trees index)))
          (format #t "Tree #~a of ~a (size ~a):~%" index (length trees) n)
          (display-tree-detailed tree)
          (format #t "Lisp form: ~a~%" (tree->lisp-form tree))))))

(define (compare-trees t1 t2)
  "Compare two trees and show their properties"
  (format #t "Tree 1: ~a~%" (tree->string t1))
  (format #t "Tree 2: ~a~%" (tree->string t2))
  (format #t "~%")
  (format #t "Size:     ~a  vs  ~a~%" (tree-size t1) (tree-size t2))
  (format #t "Height:   ~a  vs  ~a~%" (tree-height t1) (tree-height t2))
  (format #t "Width:    ~a  vs  ~a~%" (tree-width t1) (tree-width t2))
  (format #t "Leaves:   ~a  vs  ~a~%" (tree-leaves t1) (tree-leaves t2))
  (format #t "Equal:    ~a~%" (tree-equal? t1 t2)))

;;; =============================================================================
;;; Main Entry Point
;;; =============================================================================

(define (main)
  "Main demonstration of tree ontogenesis"
  (demonstrate-ontogenesis))

;; If this file is run directly
(when (and (defined? 'command-line) (not (null? (command-line))))
  (main))
