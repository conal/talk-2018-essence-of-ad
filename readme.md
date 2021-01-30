## The simple essence of automatic differentiation

*   [Invited talk for PEPM (January) 2018](https://popl18.sigplan.org/track/PEPM-2018#Invited-Talks):
    *   [Slides](http://conal.net/talks/essence-of-automatic-differentiation-2018-01.pdf) (47eK PDF).
    *   [Video (slides + audio)](https://youtu.be/Shl3MtWGu18) (60 minutes, 104MB).
*   Revised for Microsoft Research, June 2018:
    *   [Slides](http://conal.net/talks/essence-of-automatic-differentiation-2018-06.pdf) (496K PDF).
    *   Video [via microsoft.com](https://www.microsoft.com/en-us/research/video/the-simple-essence-of-automatic-differentiation/) or [directly on YouTube](https://www.youtube.com/watch?v=ne99laPUxN4) (90 minutes).
*   ICFP 2018, September 2018:
    *   [Slides](http://conal.net/talks/essence-of-automatic-differentiation-icfp.pdf) (173K PDF).
    *   [Video](https://youtu.be/MmkNSsGAZhw) (21 minutes)
*   Google, November 2018:
    *   [Slides](http://conal.net/talks/essence-of-automatic-differentiation-google.pdf) (207K PDF).
*   MIT categories seminar: *Automatic differentiation made easy via category theory*
    *   [Video](https://www.youtube.com/watch?v=17gfCTnw6uE)
    *   [Slides](http://conal.net/talks/essence-of-automatic-differentiation-mit-categories.pdf)
*   [Paper](http://conal.net/papers/essence-of-ad/) (ICFP 2018 and extended version, plus related work).

### Abstract

Automatic differentiation (AD) in reverse mode (RAD) is a central component of deep learning and other uses of large-scale optimization. Commonly used RAD algorithms such as backpropagation, however, are complex and stateful, hindering deep understanding, improvement, and parallel execution. This talk develops a simple, generalized AD algorithm calculated from a simple, natural specification. The general algorithm is then specialized by varying the representation of derivatives. In particular, applying well-known constructions to a naive representation yields two RAD algorithms that are far simpler than previously known. In contrast to commonly used RAD implementations, the algorithms defined here involve no graphs, tapes, variables, partial derivatives, or mutation. They are inherently parallel-friendly, correct by construction, and usable directly from an existing programming language with no need for new data types or programming style, thanks to use of an AD-agnostic compiler plugin.

