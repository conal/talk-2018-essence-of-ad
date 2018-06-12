## The simple essence of automatic differentiation

An [invited talk for PEPM 2018](https://popl18.sigplan.org/track/PEPM-2018#Invited-Talks).

*   [Slides (January, 2018)](http://conal.net/talks/essence-of-automatic-differentiation-2018-01.pdf) (529K PDF).
*   [Video (slides + audio)](https://youtu.be/Shl3MtWGu18) (60 minutes, 104MB).
*   [Paper](http://conal.net/papers/essence-of-ad/) (ICFP 2018).
*   [Revised slides (June, 2018)](http://conal.net/talks/essence-of-automatic-differentiation-2018-06.pdf) (529K PDF).

### Abstract

Automatic differentiation (AD) is often presented in two forms: forward mode and reverse mode.
Forward mode is quite simple to implement and package via operator overloading but is inefficient for many problems of practical interest such as deep learning and other uses of gradient-based optimization.
Reverse mode (including its specialization, back-propagation) is much more efficient for these problems, but is also typically given much more complicated explanations and implementations, involving mutation, graph construction, and "tapes".
This talk develops a very simple specification and Haskell implementation for mode-independent AD based on the vocabulary of categories (generalized functions).
Although the categorical vocabulary would be difficult to write in directly, one can instead write regular Haskell programs to be converted to this vocabulary automatically (via a compiler plugin) and then interpreted as differentiable functions.
The result is direct, exact, and efficient differentiation with no notational overhead.
The specification and implementation are then generalized considerably by parameterizing over an underlying category.
This generalization is then easily specialized to forward and reverse modes, with the latter resulting from a simple dual construction for categories.
Another instance of generalized AD is automatic incremental evaluation of functional programs, again with no notational impact to the programmer.

