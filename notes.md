## Notes

*   Describe AD:
    *   Forward mode (FAD): simple to describe and implement; performs well on low-dimensional inputs.
    *   Reverse mode (RAD):
        *   Given much more complex description, which has never sat well with me.
        *   Performs well on low-dimensional *outputs*.
        *   Foundation of much of machine learning, where it's called "back propagation".
        *   Often formulated statefully, thwarting scalability and efficient parallel execution.
*   I'll show that FAD and RAD are really special cases of one very simple, purely functional algorithm, which also specializes to mixed modes.
*   I've packaged the general AD algorithm as a GHC plugin in order to efficiently differentiate Haskell programs with no impact on programming style.

