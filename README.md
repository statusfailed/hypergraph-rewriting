# Hypergraph Rewriting

View a demo [here](https://statusfailed.github.io/miso-smc/).

An implementation of [Rewriting modulo symmetric monoidal structure](http://users.ecs.soton.ac.uk/ps/papers/rewriting.pdf)

TODO list:

- [x] Matching patterns in contexts
- [x] Rendering hypergraphs to SVG
- [x] Replacing patterns (actual rewriting)
- [ ] Parsing term expressions

Bugs

- [x] slice doesn't terminate in presence of cycles
- [x] rewrite doesn't either

Cleanup:

- [ ] remove `nub` call from taskBfs - figure out bug
- [ ] remove all "non-`VE`" variants of `neighbours`, `reachable`, and `convex`.
