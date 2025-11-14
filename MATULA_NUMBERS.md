# Matula Numbers: The Tree-Integer Bijection

## Overview

Matula numbers establish a **bijection** (one-to-one correspondence) between the set of all unlabeled rooted trees and the set of positive integers, using the fundamental structure of prime factorization.

This elegant encoding was introduced by David W. Matula in his 1968 paper "A natural rooted tree enumeration by prime factorization" (SIAM Rev. 10, 273).

## The Bijection

### Tree to Integer (Encoding)

Given a rooted tree T:

1. **Base case**: The single-vertex tree (just the root) → Matula number **1**

2. **Recursive case**: If the root has children forming subtrees T₁, T₂, ..., Tₖ with Matula numbers m₁, m₂, ..., mₖ, then:

   ```
   Matula(T) = ∏ᵢ p_{mᵢ}
   ```
   
   where p_j is the j-th prime number (p₁ = 2, p₂ = 3, p₃ = 5, ...)

### Integer to Tree (Decoding)

Given a positive integer n:

1. **If n = 1**: Return the single-vertex tree `()`

2. **Otherwise**: 
   - Compute the prime factorization: n = p₁^e₁ × p₂^e₂ × ... × pₖ^eₖ
   - For each prime factor pᵢ:
     - Find its index j (where pᵢ is the j-th prime)
     - Recursively decode j to get subtree Tⱼ
     - Add eᵢ copies of Tⱼ as children of the root

## Examples

### Small Trees

| Tree Structure | Description | Matula Number | Prime Factorization |
|:--------------|:-----------|:--------------|:-------------------|
| `()` | Single vertex (root only) | 1 | 1 |
| `(())` | Root with one child | 2 | 2¹ = p₁ |
| `((()))` | Chain of 3 nodes | 3 | 3¹ = p₂ |
| `(() ())` | Star with 2 children | 4 | 2² = p₁² |
| `(((())))` | Chain of 4 nodes | 5 | 5¹ = p₃ |
| `(() (()))` | Root with 2 different subtrees | 6 | 2 × 3 = p₁ × p₂ |
| `((() ()))` | Root with subtree of Matula 4 | 7 | 7¹ = p₄ |
| `(() () ())` | Star with 3 children | 8 | 2³ = p₁³ |
| `((()) (()))` | Root with two 2-node chains | 9 | 3² = p₂² |
| `(() ((())))` | Mixed structure | 10 | 2 × 5 = p₁ × p₃ |

### Detailed Example: Matula Number 6

**Prime factorization**: 6 = 2 × 3 = p₁ × p₂

**Decoding**:
1. Factor 2 (p₁) → index 1 → Matula(1) → tree `()`
2. Factor 3 (p₂) → index 2 → Matula(2) → tree `(())`
3. Root has two children: `()` and `(())`
4. Result: `(() (()))`

**Verification**: 
- Subtree 1: `()` → Matula 1 → prime p₁ = 2
- Subtree 2: `(())` → Matula 2 → prime p₂ = 3
- Product: 2 × 3 = 6 ✓

## Structural Patterns

### Powers of 2: Star Trees

Trees with Matula number 2^k correspond to **star graphs** (root with k single-node children):

- 2¹ = 2 → `(())` (1 child)
- 2² = 4 → `(() ())` (2 children)
- 2³ = 8 → `(() () ())` (3 children)
- 2⁴ = 16 → `(() () () ())` (4 children)

### Prime Numbers: Chains

Prime numbers correspond to specific tree structures. For prime p_k (the k-th prime), the tree is recursively built from the structure of Matula number k.

### Composite Numbers: Branched Structures

Composite numbers create trees with multiple distinct subtrees. The prime factorization directly reveals the structure:
- Each distinct prime factor creates a different type of branch
- The exponent indicates how many copies of that branch appear

## Connection to A000081

The A000081 sequence counts unlabeled rooted trees with n nodes:
```
a(n) = {0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, ...}
```

Matula numbers provide an **enumeration** of these trees:
- Trees with n nodes correspond to a specific (often sparse) subset of positive integers
- The bijection allows us to:
  - Store trees compactly as integers
  - Generate trees systematically
  - Analyze tree properties via number theory

## Implementation

### API Functions

```scheme
;; Encoding: Tree → Integer
(tree->matula tree) → integer

;; Decoding: Integer → Tree  
(matula->tree n) → tree

;; Helper functions
(nth-prime n) → integer           ; Get the n-th prime
(prime-index p) → integer         ; Get the index of prime p
(prime-factorization n) → list    ; Get prime factors as (prime . exponent) pairs
(is-prime? n) → boolean           ; Check if n is prime
```

### Tree Representation

Trees are represented as nested lists in Scheme:
- `'()` represents a single vertex (just the root)
- `'(T₁ T₂ ... Tₖ)` represents a root with k children, where each Tᵢ is a subtree

### Example Usage

```scheme
;; Convert tree to Matula number
(tree->matula '(()))              ; → 2
(tree->matula '(() (()))) ; → 6

;; Convert Matula number to tree
(matula->tree 2)                  ; → (())
(matula->tree 6)                  ; → (() (()))

;; Verify bijection
(= n (tree->matula (matula->tree n)))  ; → #t for all n > 0
```

## Mathematical Properties

### Uniqueness

The bijection is **unique** because:
1. Prime factorization is unique (Fundamental Theorem of Arithmetic)
2. Each tree has a unique recursive structure
3. The mapping preserves this structure perfectly

### Completeness

The bijection is **complete** because:
1. Every positive integer maps to exactly one tree
2. Every rooted tree maps to exactly one positive integer
3. No trees or integers are excluded

### Computability

Both directions are **computable**:
- Tree → Integer: Requires prime generation and multiplication
- Integer → Tree: Requires prime factorization and recursive construction

## Applications

### Chemical Graph Theory
Matula numbers are used to encode molecular structures as trees, enabling:
- Compact representation of molecules
- Systematic enumeration of isomers
- Computational analysis of chemical properties

### Combinatorics
The bijection provides:
- A canonical ordering of rooted trees
- Efficient algorithms for tree generation
- Connection between tree properties and number-theoretic properties

### Computer Science
Applications include:
- Abstract syntax tree (AST) encoding
- Phylogenetic tree representation
- Hierarchical data structure indexing

## Philosophical Connection to Roots

From the roots architecture perspective, Matula numbers reveal a deep truth:

**The structure of rooted trees (nested distinctions) is isomorphic to the structure of integers (prime factorization).**

This connection demonstrates:
1. **Primordial Unity**: Both trees and numbers emerge from recursive composition of primitives
2. **Hierarchical Emergence**: Complex structures (trees) map to complex numbers (composites)
3. **Computational Ontology**: The act of distinction (tree branching) corresponds to multiplicative structure (prime factorization)

Just as:
- The root is the unit (1)
- Branching corresponds to multiplication
- Prime trees correspond to prime numbers
- Composite trees correspond to composite numbers

The Matula bijection shows that **the space of all possible tree structures is equivalent to the space of positive integers**, unified by the fundamental operation of recursive composition.

## References

1. David W. Matula, "A natural rooted tree enumeration by prime factorization", SIAM Rev. 10 (1968) 273.

2. Ivan Gutman and Yeong-Nan Yeh, "Deducing properties of trees from their Matula numbers", Publications de l'Institut Mathématique (Beograd) (N.S.), Vol. 53(67), pp. 17-22 (1993).

3. Frits Göbel, "On a 1-1-correspondence between rooted trees and natural numbers", J. Combin. Theory, B 29 (1980), 141-143.

4. OEIS A000081: Number of unlabeled rooted trees with n nodes. https://oeis.org/A000081

5. OEIS A003963: Matula numbers sequence. https://oeis.org/A003963

## See Also

- `a000081.scm`: Main implementation file with Matula number functions
- `test-matula.scm`: Comprehensive test suite validating the bijection
- `roots.md`: Philosophical foundation connecting trees to computational ontology
