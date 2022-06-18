# simple-topes

[![Try prover online!](https://img.shields.io/badge/try-online!-blueviolet)](https://fizruk.github.io/simple-topes/)
[![Haddock](https://shields.io/badge/Haddock-documentation-informational)](https://fizruk.github.io/simple-topes/haddock/index.html)
[![CI status](https://github.com/fizruk/simple-topes/actions/workflows/haskell.yml/badge.svg)](https://github.com/fizruk/simple-topes/actions/workflows/haskell.yml)

Simple theorem prover for the tope layer of Riehl and Shulman type theory with shapes (RSTT) [1].

[![Try demo online!](images/simple-topes-demo.png)](https://fizruk.github.io/simple-topes/?snippet=--%20%7C%20The%20strict%20interval%20cube%20%28see%20RS17%20Section%203.1%29.%0Acube%20%F0%9D%9F%9A%20with%0A%20%20point%20%F0%9D%9F%AC%20--%20%5E%20%F0%9D%9F%AC%20point%20%28left%20point%29.%0A%20%20point%20%F0%9D%9F%AD%20--%20%5E%20%F0%9D%9F%AD%20point%20%28right%20point%29.%0A%0A--%20%7C%20Inequality%20tope%20for%20the%20strict%20interval%20cube.%0Atope%20%E2%89%A4%28%F0%9D%9F%9A%2C%20%F0%9D%9F%9A%29%20with%0A%20%20rule%20%22%28%E2%89%A4%29%20reflexivity%22%20where%0A%20%20%20%20%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%0A%20%20%20%20t%20%3A%20%F0%9D%9F%9A%20%7C%20%E2%8B%85%20%E2%8A%A2%20%E2%89%A4%28t%2C%20t%29%0A%0A%20%20rule%20%22%28%E2%89%A4%29%20transitivity%22%20where%0A%20%20%20%20t%20%3A%20%F0%9D%9F%9A%2C%20s%20%3A%20%F0%9D%9F%9A%2C%20u%20%3A%20%F0%9D%9F%9A%20%7C%20%E2%89%A4%28t%2C%20s%29%2C%20%E2%89%A4%28s%2C%20u%29%2C%20%E2%89%A4%20%28t%2C%20u%29%20%E2%8A%A2%20%CF%86%0A%20%20%20%20%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%E2%80%94%0A%20%20%20%20t%20%3A%20%F0%9D%9F%9A%2C%20s%20%3A%20%F0%9D%9F%9A%2C%20u%20%3A%20%F0%9D%9F%9A%20%7C%20%E2%89%A4%28t%2C%20s%29%2C%20%E2%89%A4%28s%2C%20u%29%20%E2%8A%A2%20%CF%86%0A%0A%09--%20TODO%3A%20add%20more%20rules%0A%0Aprove%0A%20%20t%20%3A%20%F0%9D%9F%9A%20%7C%20%E2%89%A4%28t%2C%20%F0%9D%9F%AC%29%2C%20%E2%89%A4%28%F0%9D%9F%AD%2C%20s%29%2C%20%E2%89%A4%28s%2C%20t%29%20%E2%8A%A2%20%E2%89%A4%28%F0%9D%9F%AD%2C%20t%29%20%E2%88%A7%20%E2%89%A4%28s%2C%20%F0%9D%9F%AC%29)

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
