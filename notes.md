## Notes

[*Beautiful differentiation*]: http://conal.net/papers/beautiful-differentiation/ "Paper by Conal Elliott (2009)"

*   What's a derivative?
    (Linear maps.)
*   Chain rule.
    Differentiation by itself is not compositional.
*   Pairing rule
*   Linear operations: `id`, `fst`, `snd`.
    Every linear function is its own derivative everywhere (its own perfect linear approximation).
*   Show some examples (derivatives as functions).
*   Note that AD for each operation is defined via that same operation on the derivatives (linear functions).
*   These operations correspond to cartesian categories.
    There are many others, and each might be an interesting and useful variant of AD.
    Parametrizing by a CC gives "generalized AD".
*   I'd like to generalize `NumCat` etc as well, but our definitions are specific to functions.
    Can we rephrase via a more general vocabulary?
*   Coproducts and scaling: along with products to give a generalized vocabulary of "linear maps".
    Note that the coproduct here is a cartesian product ("direct sum"), not a disjoint union.
    The `scale` operation is a scalar linear map ("$1 \times 1$ matrix"), while the `(|||)` and `(&&&)` operations grow the codomain/width and domain/height, respectively (horizontal and vertical "matrix" juxtaposition).
    These operations yield exactly the rectangular matrices.
*   Show some GAD definitions.
*   Category of functions.
    Oops. Doesn't support coproduct operations.
*   Additive functions.
*   How can we get a *data* representation?
    For instance, for machine learning via gradient descent or other forms of optimization.
    Since we know that the derivatives are *linear*, we can sample them on a basis.
    Requires $n$ passes for $n$-dimensional domain.
    Problematic in practice, since optimization problems involve gradients with high-dimensional domains (real-valued objective function) and so is particularly valuable.
*   Category of linear maps in data form.
    Generalized "matrices", i.e., compositions of representable functors.
*   Efficiency issues:
    *   We're composing many linear maps.
        Some orders of composition are more efficient than others.
    *   Fortunately, composition is associative (in *every* category), so we can associate optimally.
        Equivalent to the problem of "matrix chain multiplication", which has efficient solutions.
        Choice determined by *types*, i.e., compile-time information.
    *   All right composition gives "forward mode AD" (FAD), and all left composition gives "reverse mode AD" (RAD).
    *   FAD is best for scalar domains, and RAD best for scalar codomains.
        RAD is thus a much better choice for optimization.
*   Left-associating composition (RAD):
    *   Another angle: given a category `k`, build a wrapper category so that *every* composition pattern of the wrapper results in all-left composition for `k`.
    *   Reminiscent of a trick I learned in grad school: optimizing naive `reverse` on lists.
        The problem is left-associated list append, which is convenient but terribly costly.
        Show solution by adding an "accumulator argument", allowing for right-associate appends, reducing quadratic work to linear.
    *   Mitch Wand generalized this example and several others to a lovely pattern in [*Continuation-Based Program Transformation Strategies*], published in 1980.
        I read it a few years later at CMU, and it's been a source of inspiration ever since.
        *   Essential idea is to introduce a continuation argument and find a *data* representation for the continuations that arise in the program being transformed.
        *   The continuation representation will have an associativity property corresponding to associativity of composing continuations.
            For `reverse`, the continuations have type `[a] -> [a]`; and they all have the form `(++ as)` and so can be represented simply by `as`.
        *   This trick generalizes to all monoids, and monoids generalize to categories.
*   CPS-like category: `(b -> o) -> (a -> o)`.
    Generalize to `(b ~> r) -> (a ~> r)`, parametrized by `(~>)` and `r`.
    Corresponds to a categorical pullback and results in left-composition.
    Yields the notion of duality/transposition in linear algebra.
*   Then consider alternative representations of `z ~> r` for a given choice of `(~>)` and `r`.
    For linear (additive?) maps over a scalar `s`, `(z :-* s) =~ z`, so `b -> a`.
    This choice leads to RAD, and in particular "back propagation" as used in machine learning.

* * * * * * * * * * * * * * * * * * * *

*   Introduction:
    *   AD: computing calculus derivatives "exactly" (up to machine precision).
        Generally distinguished from symbolic differentiation (SD), but I think this distinction is overstated.
        In my view, AD *is* SD when done by a compiler and taking care to preserve sharing.
    *   Forward mode (FAD): simple to describe and implement; performs well on low-dimensional inputs.
    *   Reverse mode (RAD):
        *   Given much more complex description, which has never sat well with me.
        *   Performs well on low-dimensional *outputs*.
        *   Foundation of much of machine learning, where it's called "back propagation".
        *   Often formulated statefully, thwarting scalability and efficient parallel execution.
    *   I'll show that FAD and RAD are really special cases of one very simple, purely functional algorithm, which also specializes to mixed modes.
    *   I've packaged the general AD algorithm as a GHC plugin in order to efficiently differentiate Haskell programs with no impact on programming style.

*   Other:
    *   Differentiation:
        *   Typical definition as a limit.
        *   Chain rule via multiplication.
    *   Fixing differentiation and the chain rule:
        *   The definition of derivative and the chain rule only make sense for scalars.
            For higher-dimensional, calculus texts usually introduce the notion of "partial derivatives", and complicate the chain rule accordingly.
            Two different notions of vector-valued derivatives, Jacobians, Hessians, and other more exotic animals.
        *   The simple & general story is that derivatives are linear maps, so `a -> b :* (a :-* b)`.
            One simple & general chain rule.
    *   Simple FAD:
        *   Take from early in [*Beautiful differentiation*].
        *   Mention improvements in that paper:
            *   higher derivatives
            *   higher dimensions
        *   Criticisms:
            *   The type `Dual -> Dual` is sloppy in that (a) the result value can't depend on the argument delta, and (b) the result delta must depend *linearly* on the argument delta.
                Better: `R -> R :* R`, or `R -> R :* (R :-* R)`.
            *   Each rule contains the chain rule (as in typical calculus texts).
    *   RAD: ...

Key insights:

*   Instead of building a graph data structure, use an algebraic formulation with standard operations from category theory (including sharing/reuse).
*   Interpret the formulation in a category of differentiable functions.
*   Generate this categorical formulation from a purely functional language (Haskell).
    The categorical interfaces are type classes, and their interpretations are instances.

*   Derivatives as linear maps.
*   Two combining forms: sequential and parallel composition, with corresponding chain rule and pairing rule.
*   Linear maps form a category, and the chain and pairing rules only need categorical operations, suggesting generalized AD.

*   Because composition is associative (in all categories), we have a lot of freedom in how to apply the chain rule.
    Although all associations yield the same result, they have different computational complexities.
    (Cf matrix chain multiplication).
*   For scalar domain, right association wins; and for scalar codomain, right association wins.
    Optimization problems, including machine learning involve scalar codomains.
*   In either of these two cases, we can work with vectors only, without constructing any matrices.
    Big win, since many of these matrices would be sparse.
