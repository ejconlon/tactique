# judge

A proof search and construction framework (forked from [refinery](https://hackage.haskell.org/package/refinery))

This library allows you to define inference rules and sequence them into proof tactics to produce judgement derivation trees. It takes a lot of the core definitions from refinery, but the internals and interface have been substantially changed to yield derivations rather than fully-substituted proofs. The tactic engine is also position-aware, allowing you to traverse the in-progress derivation as you see fit. (In particular, this would enable interactive proving.)
