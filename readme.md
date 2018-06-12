## The simple essence of automatic differentiation

An [invited talk for PEPM 2018](https://popl18.sigplan.org/track/PEPM-2018#Invited-Talks).

*   [Slides (PEPM, January 2018)](http://conal.net/talks/essence-of-automatic-differentiation-2018-01.pdf) (529K PDF).
*   [Video (slides + audio)](https://youtu.be/Shl3MtWGu18) (60 minutes, 104MB).
*   [Paper](http://conal.net/papers/essence-of-ad/) (extended version of ICFP 2018 paper).
*   [Revised slides (Microsoft Research, June 2018)](http://conal.net/talks/essence-of-automatic-differentiation-2018-06.pdf) (484K PDF).

### Abstract

Automatic differentiation (AD) in reverse mode (RAD) is a central component of deep learning and other uses of large-scale optimization. Commonly used RAD algorithms such as backpropagation, however, are complex and stateful, hindering deep understanding, improvement, and parallel execution. This talk develops a simple, generalized AD algorithm calculated from a simple, natural specification. The general algorithm is then specialized by varying the representation of derivatives. In particular, applying well-known constructions to a naive representation yields two RAD algorithms that are far simpler than previously known. In contrast to commonly used RAD implementations, the algorithms defined here involve no graphs, tapes, variables, partial derivatives, or mutation. They are inherently parallel-friendly, correct by construction, and usable directly from an existing programming language with no need for new data types or programming style, thanks to use of an AD-agnostic compiler plugin.

