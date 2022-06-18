# simple-topes

[![Haddock](https://shields.io/badge/Haddock-documentation-informational)](https://fizruk.github.io/simple-topes/haddock/index.html)
[![CI status](https://github.com/fizruk/simple-topes/actions/workflows/haskell.yml/badge.svg)](https://github.com/fizruk/simple-topes/actions/workflows/haskell.yml)

Simple theorem prover for the tope layer of Riehl and Shulman type theory with shapes (RSTT) [1].

```haskell
prove
  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ (φ ∨ ζ) ∧ (ψ ∨ χ)
```

```
[∧R]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ (φ ∨ ζ) ∧ (ψ ∨ χ)
├─ [∨L]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ φ ∨ ζ
│  ├─ [∧L]  ⋅ | φ ∧ ψ ⊢ φ ∨ ζ
│  │  └─ [∨R₁]  ⋅ | φ, ψ ⊢ φ ∨ ζ
│  │     └─ [Ax]  ⋅ | φ, ψ ⊢ φ
│  └─ [∧L]  ⋅ | ζ ∧ χ ⊢ φ ∨ ζ
│     └─ [∨R₂]  ⋅ | ζ, χ ⊢ φ ∨ ζ
│        └─ [Ax]  ⋅ | ζ, χ ⊢ ζ
└─ [∨L]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ ψ ∨ χ
   ├─ [∧L]  ⋅ | φ ∧ ψ ⊢ ψ ∨ χ
   │  └─ [∨R₁]  ⋅ | φ, ψ ⊢ ψ ∨ χ
   │     └─ [Ax]  ⋅ | φ, ψ ⊢ ψ
   └─ [∧L]  ⋅ | ζ ∧ χ ⊢ ψ ∨ χ
      └─ [∨R₂]  ⋅ | ζ, χ ⊢ ψ ∨ χ
         └─ [Ax]  ⋅ | ζ, χ ⊢ χ
```

## About

This is an experimental project, related to [rzk](https://github.com/fizruk/rzk) proof assistant.
The tope layer in RSTT serves as a tool to specify higher-dimensional diagrams (in particular for (∞,1)-categories).
However, it appears that in practice (at least for the proofs in RSTT)
statements about topes are fairly straightforward and should __always__ be solved automatically.
Moreover, it seems that the prover need not be very efficient either, since the proof search space is supposed to be relatively small.

One complication, however, is that users can define their own cubes, points, topes, together with their own tope axioms.
These new rules should not complicate matters too much, but I have yet to figure out what assumptions are safe to make about user-defined axioms.

Once stable, this implementation will likely be incorporated into [rzk](https://github.com/fizruk/rzk) proof assistant.

## Development

For quick local development and testing it is recommended to work with [Stack tool](https://docs.haskellstack.org/en/stable/README/).
Clone this project and simply run `stack build`:

```sh
git clone git@github.com:fizruk/simple-topes.git
cd rzk
stack build
```

# References

1.  Emily Riehl and Michael Shulman. __A type theory for synthetic ∞-categories.__ _Higher Structures, 1, 2017._ https://arxiv.org/abs/1705.07442
