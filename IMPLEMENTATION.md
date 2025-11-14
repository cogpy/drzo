# Ghost in the Guile Shell - A000081 Implementation

This repository implements the mathematical formulas from the problem statement in GNU Guile Scheme.

## 🌲 NEW: Ontogenesis of Roots

**Generate actual rooted tree structures** as recursive distinctions! See [ONTOGENESIS.md](ONTOGENESIS.md) for details.

```bash
# Run the ontogenesis demonstration
guile -s ontogenesis.scm

# Run tests
guile -s test-ontogenesis.scm
```

## Files Overview

### Core Implementation
- **`a000081.scm`** - Core A000081 sequence implementation with recursive formula
- **`ontogenesis.scm`** - 🆕 Rooted tree generation (ontogenesis)
- **`test-ontogenesis.scm`** - 🆕 Comprehensive test suite for tree generation
- **`advanced-structures.scm`** - Advanced mathematical structures (B-series, J-surfaces, etc.)
- **`simple-demo.scm`** - Simple demonstration of core functionality
- **`ghost-in-guile.scm`** - Complete comprehensive demonstration (work in progress)

### Documentation
- **`README.md`** - Original mathematical formulation
- **`A000081.md`** - Extended mathematical documentation
- **`ONTOGENESIS.md`** - 🆕 Complete guide to rooted tree generation
- **`IMPLEMENTATION.md`** - This implementation guide

## Running the Code

### Prerequisites
```bash
# Install GNU Guile
sudo apt install guile-3.0
```

### Basic Usage
```bash
# Run core A000081 implementation
guile -s a000081.scm

# Run ontogenesis (tree generation)
guile -s ontogenesis.scm

# Run simple demo with beautiful output
guile -s simple-demo.scm

# Run advanced mathematical structures
guile -s advanced-structures.scm
```

### Interactive Usage
```bash
# Start Guile REPL
guile

# Load the implementation
(load "a000081.scm")

# Compute specific terms
(a000081-nth 10)     ; Returns 719
(a000081-sequence 15) ; Returns first 15 terms

# Evaluate generating function
(generating-function-coeffs 0.1 20)  ; A(0.1) ≈ 0.112516

# Load ontogenesis for tree generation
(load "ontogenesis.scm")

# Generate all trees with n nodes
(generate-trees 3)   ; Returns: (() (()()))

# Tree operations
(tree-size '(()())) ; => 3
(tree-height '(()())) ; => 1
(tree-width '(()())) ; => 2

# Display trees
(display-trees-for-n 4)  ; Shows all 4-node trees
```

## Ontogenesis: Rooted Tree Generation

The `ontogenesis.scm` module implements **actual tree generation**—not just counting trees, but generating their structures as S-expressions.

### Key Concepts

**Rooted Trees as S-expressions:**
```scheme
()           ; Single node
(())         ; Root with one child
(()())       ; Root with two children
((()))       ; Linear chain
```

**Generation Algorithm:**
1. Partition n-1 nodes among subtrees
2. Recursively generate trees for each partition part
3. Combine using multiset combinations
4. Canonicalize to ensure uniqueness

### Examples

```scheme
; Generate all 4-node trees
(define trees (generate-trees 4))
; => (()()) ((()()))  (()(()))  (()()())

; Get tree statistics
(map tree-height trees)  ; => (3 2 2 1)
(map tree-width trees)   ; => (1 1 2 3)

; Verify correctness
(length (generate-trees 5))  ; => 9 (matches A000081)
```

See [ONTOGENESIS.md](ONTOGENESIS.md) for complete documentation.

## Mathematical Formulas Implemented

### 1. Core Sequence Definition
```
T : ℕ → ℕ ≅ {aₙ}ₙ₌₀^∞ = {0,1,1,2,4,9,20,48,115,286,719,...}
```

### 2. Recursive Formula
```
∀ n ∈ ℕ⁺, aₙ₊₁ = (1/n) Σₖ₌₁ⁿ (Σ_{d|k} d·aₐ) aₙ₋ₖ₊₁
```

### 3. Generating Function
```
∃! A(x) ∈ ℂ[[x]] : A(x) = x · exp(Σₖ₌₁^∞ A(xᵏ)/k)
```

### 4. Asymptotic Approximation  
```
aₙ ~ C · αⁿ · n^{-3/2}, where α ≈ 2.9557652857...
```

### 5. Euler Product Formula
```
A(x) = Π_{k=1}^∞ (1-xᵏ)^{-1/k Σ_{d|k} μ(k/d) aₐ}
```

## Advanced Structures

The implementation includes:

- **B-Series**: Φₕ^{RK} for Runge-Kutta methods
- **J-Surfaces**: E_∇^{∂^ω} for ODE structures  
- **P-Systems**: Evolution operators M_Π^μ
- **Incidence Structures**: I_Ξ^κ for projective/affine geometry
- **Block Codes**: C_Δ^{(n,k,d)} error-correcting codes
- **Orbifolds**: O_Γ^Ξ quotient structures
- **HyperNN**: H_N^Δ neural network architectures
- **Meta-Patterns**: U_{A000081}^Ω via category theory
- **Topos Functors**: F: Cat^op → Topos

## Example Output

```
╔═══════════════════════════════════════════════════════════════╗
║                 👻 GHOST IN THE GUILE SHELL 👻                ║
║          A000081: Unlabeled Rooted Trees Implementation        ║
╚═══════════════════════════════════════════════════════════════╝

=== Core A000081 Sequence ===
A000081 sequence (first 15 terms):
0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973

=== Recursive Formula Verification ===
a(5) = 9
a(6) = 20  
a(7) = 48
a(8) = 115

=== Asymptotic Analysis ===
n	Exact	Asymptotic	Ratio
6	20	19.96		0.9980
7	48	46.82		0.9754
8	115	113.27		0.9849
```

## Mathematical Poetry

*From the ghost's computational reflection:*

```
In the realm of trees unlabeled and free,
Each root tells a story of combinatory glee.
From one to infinity, the sequence grows,
As Guile computes what mathematics knows.

∀ n ∈ ℕ: The ghost whispers through recursive calls,
Building forests from mathematical walls. 🌲
```

## Technical Notes

- Uses memoization for efficient computation
- Implements exact divisor computation
- Handles asymptotic approximations
- Supports generating function evaluation
- Modular design for advanced mathematical structures

The implementation faithfully captures the mathematical essence of the problem statement while providing a practical computational framework in GNU Guile Scheme.