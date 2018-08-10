## Revision notes

*   Make a new first slide giving motivation for AD and my work in particular.
*   Slides 28, 29: streamline.
*   Maybe use `Monoidal`/`(***)` rather than `Cartesian`/`(&&&)`.
    I'd have to define `(&&&)` and `(|||)`.

Done:

*   Slide 6: drop "i.e."
*   Slide 7: drop product subscript (not used elsewhere).
*   Slide 8: side-by-side homomorphisms
*   Slide 13: add CCC expression.
*   Slides 8, 16, ...: Drop extra parens? Or move them to around the second fork argument.
*   Slide 24: use superscript and subscript (both as in paper)
*   Slide 26: simplify.
*   Slide 28: Replace `r` by `s`.
*   Slide 31: break slide in two, with specification and solution.
*   Slide 32: drop "`Double`". Other slides, too?
*   Slide 37: correct picture? [yes]
*   The functorial specification.
    Somewhere: point out that all of the algorithms are calculated from simple specifications as homomorphisms, referring to the paper.

## To do

*   `CoproductPCat` for `GD k`.
    The "cocartesian rule", a peer to the chain and cartesian rules.

## Did

*   `AddFun`
*   Abstract
*   Use infix `(~>)` in place of `k`.
*   Rework `Incremental`.
*   Resolve whether to give the talk at PEPM.
    [Yes](https://popl18.sigplan.org/track/PEPM-2018#Invited-Talks).
*   AD example `cosSinProd`
*   Extracting a data representation: if we use `AddFun`, we have an expensive extraction step e.g., to get the gradient.
*   RAD examples with derivatives as dual functions and as dual vectors: `add`, `fst`, `sqr`, `magSqr`, `cosSinProd`.
*   Get the talk scheduled at work. Wed 3pm.
*   Add `Incremental` to talk.
*   Remarks on symbolic vs automatic differentiation
*   Maybe show CPS-like category (`ConCat.Continuation`, in progress).
*   Conclusions: FAD & RAD ...
*   Linear maps via representable functors.
*   Perhaps material from second set of notes
