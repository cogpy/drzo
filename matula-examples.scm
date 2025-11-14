#!/usr/bin/env guile
!#

;;; Practical examples of using Matula numbers
;;; Demonstrates real-world applications of the tree-integer bijection

(use-modules (srfi srfi-1)
             (ice-9 format))

;; Load the main module
(load "a000081.scm")

(define (separator msg)
  "Print a section separator with message"
  (format #t "~%~a~%" (make-string 70 #\=))
  (format #t "~a~%" msg)
  (format #t "~a~%~%" (make-string 70 #\=)))

;;; Example 1: Compact tree storage
(separator "EXAMPLE 1: Compact Tree Storage")

(format #t "Trees can be stored as integers instead of complex nested structures:~%~%")

(define sample-trees
  '((())                    ; Simple chain
    (() () ())              ; Star with 3 branches
    (() ((()) ()))          ; Mixed structure
    (((())) (() ()))))      ; Complex branching

(format #t "~30@a │ ~15@a │ ~20@a~%"
        "Tree Structure" "Matula Number" "Storage Benefit")
(format #t "~a~%" (make-string 70 #\─))

(for-each
 (lambda (tree)
   (let* ((matula (tree->matula tree))
          (tree-str (format #f "~a" tree))
          (matula-str (format #f "~a" matula))
          (saved (- (string-length tree-str) (string-length matula-str))))
     (format #t "~30@a │ ~15@a │ ~20@a~%"
             tree matula (format #f "~a bytes saved" saved))))
 sample-trees)

(format #t "~%Benefit: Storing integers is more efficient than nested lists!~%")

;;; Example 2: Tree enumeration and search
(separator "EXAMPLE 2: Systematic Tree Enumeration")

(format #t "Generate all rooted trees with exactly n nodes:~%~%")

(define (count-nodes tree)
  "Count nodes in a tree"
  (if (null? tree)
      1
      (+ 1 (apply + (map count-nodes tree)))))

(define (trees-with-n-nodes n limit)
  "Find all Matula numbers for trees with n nodes, up to limit"
  (let loop ((m 1) (found '()))
    (if (> m limit)
        (reverse found)
        (let ((tree (matula->tree m)))
          (if (= (count-nodes tree) n)
              (loop (+ m 1) (cons m found))
              (loop (+ m 1) found))))))

(do ((n 1 (+ n 1)))
    ((> n 6))
  (let ((matulas (trees-with-n-nodes n 100)))
    (format #t "Trees with ~a nodes: ~{~a~^ ~}~%"
            n (if (> (length matulas) 8)
                  (append (take matulas 8) '(...))
                  matulas))))

(format #t "~%Benefit: Systematically enumerate trees by incrementing integers!~%")

;;; Example 3: Tree comparison and equality
(separator "EXAMPLE 3: Fast Tree Comparison")

(format #t "Compare trees by comparing their Matula numbers:~%~%")

(define test-pairs
  '(((()) (()))
    ((() ()) (() ()))
    ((() ()) ((()) ()))
    ((() () ()) (() () () ()))))

(format #t "~20@a │ ~20@a │ ~12@a │ ~a~%"
        "Tree 1" "Tree 2" "Matula Match" "Result")
(format #t "~a~%" (make-string 70 #\─))

(for-each
 (lambda (pair)
   (let* ((tree1 (car pair))
          (tree2 (cadr pair))
          (m1 (tree->matula tree1))
          (m2 (tree->matula tree2))
          (match (= m1 m2)))
     (format #t "~20@a │ ~20@a │ ~6@a = ~4@a │ ~a~%"
             tree1 tree2 m1 m2 (if match "Same" "Different"))))
 test-pairs)

(format #t "~%Benefit: O(1) comparison instead of recursive tree traversal!~%")

;;; Example 4: Finding tree properties
(separator "EXAMPLE 4: Deducing Tree Properties from Matula Numbers")

(format #t "Number-theoretic properties reveal tree structure:~%~%")

(define (is-star-tree? n)
  "Check if Matula number represents a star tree (power of 2)"
  (and (> n 1)
       (= (length (prime-factorization n)) 1)
       (= (car (car (prime-factorization n))) 2)))

(define (is-chain? n)
  "Check if Matula number represents a chain (prime number)"
  (is-prime? n))

(define (subtree-count n)
  "Count the number of distinct subtrees from Matula factorization"
  (if (= n 1)
      0
      (length (prime-factorization n))))

(format #t "~10@a │ ~15@a │ ~10@a │ ~15@a~%"
        "Matula" "Prime Factor" "Star Tree?" "Chain?")
(format #t "~a~%" (make-string 70 #\─))

(do ((n 1 (+ n 1)))
    ((> n 15))
  (let* ((factors (if (= n 1) '() (prime-factorization n)))
         (factor-str (if (null? factors)
                        "1"
                        (string-join
                         (map (lambda (p)
                                (if (= (cdr p) 1)
                                    (format #f "~a" (car p))
                                    (format #f "~a^~a" (car p) (cdr p))))
                              factors)
                         "×"))))
    (format #t "~10@a │ ~15@a │ ~10@a │ ~15@a~%"
            n factor-str
            (if (is-star-tree? n) "Yes" "No")
            (if (is-chain? n) "Yes" "No"))))

(format #t "~%Benefit: Instantly determine tree properties without traversal!~%")

;;; Example 5: Tree generation by constraints
(separator "EXAMPLE 5: Generating Trees by Constraints")

(format #t "Find trees matching specific criteria:~%~%")

(define (find-trees-by-height height limit)
  "Find trees with specific height"
  (define (tree-height tree)
    (if (null? tree)
        0
        (+ 1 (apply max 0 (map tree-height tree)))))
  
  (let loop ((m 1) (found '()))
    (if (> m limit)
        (reverse found)
        (let ((tree (matula->tree m)))
          (if (= (tree-height tree) height)
              (loop (+ m 1) (cons m found))
              (loop (+ m 1) found))))))

(do ((h 1 (+ h 1)))
    ((> h 4))
  (let ((trees (find-trees-by-height h 50)))
    (format #t "Height ~a trees: ~{~a~^ ~}~%"
            h (if (> (length trees) 6)
                  (append (take trees 6) '(...))
                  trees))))

(format #t "~%Benefit: Filter trees by properties efficiently!~%")

;;; Example 6: Tree transformation
(separator "EXAMPLE 6: Tree Transformations via Number Operations")

(format #t "Transform trees by manipulating their Matula numbers:~%~%")

(define (add-single-child tree)
  "Add a single-vertex subtree as a new child"
  (let ((m (tree->matula tree)))
    ;; Multiply by p₁ = 2 to add a single-vertex child
    (matula->tree (* m 2))))

(define (replicate-structure tree times)
  "Create a root with 'times' copies of the given tree"
  (let ((m (tree->matula tree)))
    ;; p_m^times creates a tree with 'times' copies of tree as children
    (matula->tree (expt (nth-prime m) times))))

(format #t "Original tree: (())~%")
(format #t "  Matula: ~a~%" (tree->matula '(())))
(format #t "~%")

(format #t "After adding single child: ~a~%" (add-single-child '(())))
(format #t "  Matula: ~a~%" (tree->matula (add-single-child '(()))))
(format #t "~%")

(format #t "Replicate 3 times: ~a~%" (replicate-structure '(()) 3))
(format #t "  Matula: ~a~%" (tree->matula (replicate-structure '(()) 3)))
(format #t "~%")

(format #t "Benefit: Tree operations become arithmetic operations!~%")

;;; Example 7: Tree database indexing
(separator "EXAMPLE 7: Tree Database with Integer Keys")

(format #t "Use Matula numbers as database keys for tree structures:~%~%")

;; Simple in-memory tree database
(define tree-db (make-hash-table))

(define (db-store! tree metadata)
  "Store a tree with metadata using its Matula number as key"
  (let ((key (tree->matula tree)))
    (hash-set! tree-db key (cons tree metadata))
    key))

(define (db-lookup key)
  "Retrieve tree and metadata by Matula number"
  (hash-ref tree-db key #f))

;; Store some trees with metadata
(db-store! '(()) '("name" "simple-chain" "type" "linear"))
(db-store! '(() ()) '("name" "fork" "type" "branching"))
(db-store! '((()) (())) '("name" "symmetric" "type" "balanced"))

(format #t "Database contents:~%")
(format #t "~10@a │ ~20@a │ ~30@a~%"
        "Key" "Tree" "Metadata")
(format #t "~a~%" (make-string 70 #\─))

(hash-for-each
 (lambda (key value)
   (format #t "~10@a │ ~20@a │ ~30@a~%"
           key (car value) (cdr value)))
 tree-db)

(format #t "~%Benefit: Trees indexed by integers for fast lookup!~%")

;;; Conclusion
(separator "CONCLUSION")

(format #t "Matula numbers enable:~%")
(format #t "  1. Compact storage (integers vs nested structures)~%")
(format #t "  2. Systematic enumeration (iterate through integers)~%")
(format #t "  3. Fast comparison (integer equality)~%")
(format #t "  4. Property deduction (via prime factorization)~%")
(format #t "  5. Constraint-based generation (filter integers)~%")
(format #t "  6. Algebraic transformations (arithmetic operations)~%")
(format #t "  7. Database indexing (integer keys)~%")
(format #t "~%")
(format #t "This bijection is a powerful tool for working with tree structures!~%~%")
