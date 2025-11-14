# Matula Numbers Quick Start Guide

## What Are Matula Numbers?

Matula numbers create a **one-to-one correspondence** between rooted trees and positive integers using prime factorization.

```
Tree    ←→    Integer
  ()    ←→       1
 (())   ←→       2
((()))  ←→       3
(() ()) ←→       4
```

## How It Works

### Encoding: Tree → Integer

1. Single vertex tree `()` → `1`
2. For any other tree, get the Matula number of each child subtree
3. For subtree with Matula number `m`, use the `m`-th prime `p_m`
4. Multiply all these primes together

**Example:** Tree `(() (()))`
- Left child `()` → Matula 1 → prime p₁ = 2
- Right child `(())` → Matula 2 → prime p₂ = 3  
- Result: 2 × 3 = **6**

### Decoding: Integer → Tree

1. If `n = 1` → single vertex tree `()`
2. Otherwise, factor `n` into primes
3. For each prime factor `p`:
   - Find its index `i` (where `p` is the `i`-th prime)
   - Recursively decode `i` to get a subtree
4. Collect all subtrees as children of the root

**Example:** Matula number 6
- Factor: 6 = 2 × 3
- Prime 2 is p₁ (index 1) → Matula(1) → tree `()`
- Prime 3 is p₂ (index 2) → Matula(2) → tree `(())`
- Result: Root with children `()` and `(())` = **`(() (()))`**

## Quick Start

### Installation

```bash
# Install Guile Scheme (Ubuntu/Debian)
sudo apt-get install guile-3.0

# Clone repository
git clone https://github.com/cogpy/drzo.git
cd drzo
```

### Basic Usage

```scheme
#!/usr/bin/env guile
!#

;; Load the Matula numbers implementation
(load "a000081.scm")

;; Convert tree to Matula number
(define my-tree '(() (())))
(define m (tree->matula my-tree))
(format #t "Tree ~a → Matula ~a~%" my-tree m)
; Output: Tree (() (())) → Matula 6

;; Convert Matula number to tree
(define tree (matula->tree 10))
(format #t "Matula 10 → Tree ~a~%" tree)
; Output: Matula 10 → Tree (() ((())))

;; Verify bijection
(format #t "Round-trip: ~a~%" 
        (= m (tree->matula (matula->tree m))))
; Output: Round-trip: #t
```

### Run Examples

```bash
# Run main implementation
guile a000081.scm

# Run test suite (40+ tests)
guile test-matula.scm

# Run interactive demo
guile matula-demo.scm

# Run practical examples
guile matula-examples.scm
```

## API Reference

### Core Functions

```scheme
;; Encoding
(tree->matula tree) → integer
  ; Convert tree (as nested list) to Matula number

;; Decoding  
(matula->tree n) → tree
  ; Convert Matula number to tree structure

;; Prime utilities
(nth-prime n) → integer
  ; Get the n-th prime (1st prime = 2)

(prime-index p) → integer
  ; Get index of prime p

(prime-factorization n) → list
  ; Get list of (prime . exponent) pairs

(is-prime? n) → boolean
  ; Check if n is prime

;; Validation
(validate-matula-bijection n) → boolean
  ; Verify that tree→matula→tree = identity for n
```

### Tree Representation

Trees are represented as nested lists:
- `'()` = single vertex (just the root)
- `'(T1 T2 ... Tk)` = root with k children

Examples:
```scheme
'()              ; Single vertex
'(())            ; Root with one child
'(() ())         ; Root with two children
'((()) ())       ; Root with two children, left one has a child
'(((())))        ; Linear chain of 4 nodes
```

## Common Patterns

### Powers of 2 = Star Trees

```scheme
(matula->tree 2)   ; → (())          (1 child)
(matula->tree 4)   ; → (() ())       (2 children)
(matula->tree 8)   ; → (() () ())    (3 children)
(matula->tree 16)  ; → (() () () ()) (4 children)
```

### Prime Numbers = Special Structures

```scheme
(matula->tree 2)   ; → (())         (prime 2)
(matula->tree 3)   ; → ((()))       (prime 3)
(matula->tree 5)   ; → (((())))     (prime 5)
(matula->tree 7)   ; → ((() ()))    (prime 7)
```

### Composite = Mixed Branches

```scheme
(matula->tree 6)   ; → (() (()))    (2 × 3)
(matula->tree 10)  ; → (() ((())))  (2 × 5)
(matula->tree 12)  ; → (() () (())) (2² × 3)
```

## Use Cases

### 1. Compact Storage
Store trees as integers instead of nested structures:
```scheme
;; Tree: (() () ())
;; As list: 11 characters
;; As Matula: "8" = 1 character
;; Savings: 10 bytes (91%)
```

### 2. Fast Comparison
```scheme
;; Compare two trees
(= (tree->matula tree1) (tree->matula tree2))
;; O(1) instead of O(n) recursive comparison
```

### 3. Systematic Enumeration
```scheme
;; Generate all trees by iterating integers
(map matula->tree (iota 10 1))  ; First 10 trees
```

### 4. Property Analysis
```scheme
;; Check if tree is a "star" (power of 2)
(define (star-tree? n)
  (and (> n 1)
       (= 1 (length (prime-factorization n)))
       (= 2 (car (car (prime-factorization n))))))
```

## Example: Finding Trees

```scheme
;; Find all trees with exactly 4 nodes
(define (count-nodes tree)
  (if (null? tree)
      1
      (+ 1 (apply + (map count-nodes tree)))))

(define (trees-with-4-nodes limit)
  (filter (lambda (n)
            (= 4 (count-nodes (matula->tree n))))
          (iota limit 1)))

(trees-with-4-nodes 20)
; → (5 6 7 8) - Four different 4-node trees
```

## Verification

```scheme
;; Test bijection property
(do ((n 1 (+ n 1)))
    ((> n 100))
  (unless (= n (tree->matula (matula->tree n)))
    (error "Bijection failed!" n)))

;; All 100 tests pass! ✓
```

## Mathematical Background

The bijection relies on:
1. **Unique prime factorization** (Fundamental Theorem of Arithmetic)
2. **Recursive tree structure** (each node defines subtrees)
3. **Prime indexing** (each Matula number indexes a prime)

This creates a **perfect one-to-one mapping** where:
- Every tree has exactly one Matula number
- Every positive integer corresponds to exactly one tree
- The mapping is efficiently computable in both directions

## Further Reading

- **MATULA_NUMBERS.md** - Comprehensive mathematical explanation
- **INTEGRATION_SUMMARY.md** - Project implementation details
- **.github/agents/roots.md** - Philosophical connections

## Quick Reference Card

| Matula | Tree Structure | Description |
|--------|---------------|-------------|
| 1 | `()` | Single vertex |
| 2 | `(())` | Root + 1 child |
| 3 | `((()))` | Chain of 3 |
| 4 | `(() ())` | Star (2 children) |
| 5 | `(((())))` | Chain of 4 |
| 6 | `(() (()))` | Mixed structure |
| 7 | `((() ()))` | Nested structure |
| 8 | `(() () ())` | Star (3 children) |
| 9 | `((()) (()))` | Two 2-chains |
| 10 | `(() ((())))` | Mixed: 1 + 3-chain |

## Support

For questions or issues:
1. Check the test suite: `test-matula.scm`
2. Run the demos: `matula-demo.scm`, `matula-examples.scm`
3. Read the full documentation: `MATULA_NUMBERS.md`

---

**Ready to explore the tree-integer bijection!** 🌳 ↔️ 🔢
