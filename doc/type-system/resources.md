# What is this file?

This file contains links and resources for learning the type system in sslang.

# Resources

1. [write you a haskell](https://smunix.github.io/dev.stephendiehl.com/fun/index.html)

In particular, chapter 7 builds a simple Hindley-Milner type system in 2 approaches - one is based on Algorithm W, the other is based on an extremely simple constraint solving approach. The book is well-written and easy to read, and should serve as an intuition-builder. However, the chapter should not serve as a reference implementation, for the following reason:

- Both approaches uses substitutions to build unifiers, which is not efficient.
- The constraint-based approach uses constraints that are too simple, inefficient or possibly even not correct (I can't guarantee its theoretical correctness).

Overall, this book is recommended as an intro-to-type-system read.

2. [Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/thih.pdf)

A paper with reference code that type checks a tiny, core fragment of Haskell. It is basically Hindley-Milner’s Algorithm W by using substitutions to build unifiers. We are not taking this approach.

This is an okay resource to build intuition.

3. [Implementing Hindley-Milner with the unification-fd library](https://byorgey.wordpress.com/2021/09/08/implementing-hindley-milner-with-the-unification-fd-library/)

A blog post that uses Haskell’s `unification-fd` library to perform Hindley-Milner type inference. This approach uses first-order unification instead of substitutions to build unifiers, which is the more efficient approach that we are using. The current implemention of sslang’s type inference is similar to this, located in `src/IR/Types`.

4. [The Essence of ML Type Inference](http://gallium.inria.fr/~fpottier/publis/emlti-final.pdf)

This is Chapter 10 of Advanced Topics in Types and Programming Languages (aka. ATTAPL). It introduces Hindley-Milner as a constraint-based type system, and proposes an inference procedure for it (constraint generation + constraint solving). It also discusses some extension to the type system, including recursive functions, recursive types, ADTs and row types. This is an extremely rigorous piece of writing on constraint-based HM type system, and I would only recommend reading this if you want a _BIG_ challenge. The reference implementation, called mini, can be found at this [webpage](http://cristal.inria.fr/attapl/).

5. [Hindley-Milner Elaboration in Applicative Style](http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf)

There are important differences in type-checking, type-inference and type-elaboration. What we really want for sslang is _elaboration_. This paper by Francois Pottier extends the constraint-based type-inference algorithm in (4. The Essence of ML Type Inference) to be an elaboration procedure. It is a well-written paper that explains the intuition and semantics of constraints, and goes through how they can be modified to perform elaboration. The paper is also presented by Pottier as a [Functional Pearl talk](https://www.youtube.com/watch?v=8b79M4Nmh34&ab_channel=MalcolmWallace). There is an up-to-date reference OCaml library implementation called [inferno](https://gitlab.inria.fr/fpottier/inferno). There is also an old port to Haskell by Stephanie Wierich called [hs-inferno](https://github.com/sweirich/hs-inferno).

This is the paper that the alternate sslang type system is based on. The code is located in `src/Constraint`. The implementation is heavily influenced by the `inferno` OCaml library.
