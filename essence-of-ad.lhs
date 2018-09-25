%% -*- latex -*-

% Presentation
%\documentclass[aspectratio=1610]{beamer} % Macbook Pro screen 16:10
%% \documentclass{beamer} % default aspect ratio 4:3
\documentclass[handout]{beamer}

% \setbeameroption{show notes} % un-comment to see the notes

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

\title[]{The simple essence of automatic differentiation}
% \subtitle{(Differentiable programming made easy)}
\date{
%if icfp
ICFP, September 2018
%else
January/June 2018
%endif
}
% \date{\today{} (draft)}
\institute[]{Target}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{1ex}
\setlength{\blanklineskip}{1.5ex}
\setlength\mathindent{4ex}

\nc\wow\emph

\usepackage{scalerel}

%format == = =

\begin{document}

% \large

\frame{\titlepage}
% \title{Essence of automatic differentiation}
\title{Simple essence of AD}
% \institute{Target}
\date{
%if icfp
ICFP 2018
%else
January/June 2018
%endif
}

%format der = "\mathcal{D}"

%if True
\framet{Machine learning: promise and problems}{ % Motivation

\parskip2ex

% Current AI revolution runs on large data, speed, and AD\pause, but
Impressive results.\pause\hspace{-3pt}, but

\begin{itemize}\itemsep3ex
\item AD algorithm (backprop) is complex and stateful.
\item Complex graph APIs.
\end{itemize}

\vspace{2ex}

\pause
%% Differentiable programming made easy:
Paper's contributions:

\begin{itemize}\itemsep3ex
%% \item Simple, generalized, efficient, parallel-friendly, \emph{calculated} AD.
%% \item Use host language instead of API.
\item AD: Simple, calculated, efficient, parallel-friendly, generalized.
\item API: |derivative|.
\end{itemize}
}
%endif

%if icfp
\framet{Compiling to categories}{
\begin{itemize}\itemsep2ex\parskip1.5ex
\item
  Presented at ICFP 2017.
\item
  A GHC plugin:
  \begin{itemize}\itemsep3ex
  \item
    Convert regular Haskell code to categorical form.
  \item
    Reinterpret in other categories.
  \end{itemize}
%% \item
%%   Solve homomorphisms for correct implementations.
%% \item
%%   Works even for non-computable homomorphisms (e.g., differentiation).
\end{itemize}
}
%endif

%if not icfp
\framet{What's a derivative?}{
\begin{itemize}\itemsep4ex
\pitem Number
\pitem Vector
\pitem Covector
\pitem Matrix
\pitem Higher derivatives
\end{itemize}

\vspace{2ex}\pause
Chain rule for each.
}
%endif

\framet{Differentiation}{\mathindent20ex
%if not icfp
\pause
%endif
\vspace{2ex}

> der :: (a -> b) -> (a -> (a :-* b))

%% \pause

%if not icfp
producing a local linear approximation:

\ 

$$|lim(epsilon -> 0)(frac(norm (f (a+epsilon) - (f a + der f a epsilon)))(norm epsilon)) == 0|$$

\vspace{8ex}

See \href{https://archive.org/details/SpivakM.CalculusOnManifolds_201703}{\emph{Calculus on Manifolds}} by Michael Spivak.
%endif
}


\framet{Composition}{

Sequential:

\begin{code}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(g . f) a = g (f a)

NOP
der (g . f) a == der g (f a) . der f a    -- chain rule
\end{code}

\pause

Parallel:

\begin{code}
(&&&) :: (a -> c) -> (a -> d) -> (a -> c :* d)
(f &&& g) a = (f a, g a)

NOP
der (f &&& g) a == der f a &&& der g a
\end{code}

}

%format adf = "\hat{"der"}"

\framet{Linear functions}{

%if not icfp
\pause
%endif
Linear functions are their own derivatives everywhere.
% perfect linear approximations.

\vspace{2ex}

\begin{code}
der id   a  =  id
der fst  a  =  fst
der snd  a  =  snd
            ...
\end{code}

%if False
%% After swapping slides, drop this part
For linear functions |f|,

> adf f a = (f a, f)

%% i.e.,

%% > adf f = f &&& const f
%endif
}

\framet{Compositionality}{

Chain rule:

> der (g . f) a == der g (f a) . der f a     -- non-compositional

\ 

%if not icfp
\pause
%endif
To fix, combine regular result with derivative:
\begin{code}
adf :: (a -> b) -> (a -> (b :* (a :-* b)))
adf f = f &&& der f     -- specification
\end{code}
% adf f = (f a, der f a)    -- specification

%if not icfp
Often much work in common to |f| and |der f|.
%endif
}

%% \nc\scrk[1]{_{\hspace{#1}\scriptscriptstyle{(\leadsto)\!}}}
\nc\scrk[1]{_{\hspace{#1}\scaleto{(\leadsto)\!}{4pt}}}

%format ProductCat = Cartesian
%format CoproductCat = Cocartesian
%format CoproductPCat = Cocartesian

%format Prod (k) a b = a "\times\scrk{-0.4ex}" b
%format Coprod (k) a b = a "+\scrk{-0.4ex}" b
%% %format Exp (k) a b = a "\Rightarrow\scrk{-0.2ex}" b

%%  %format da = "\Delta a"
%%  %format db = "\Delta b"

%format `k` = "\leadsto"
%format k = "(\leadsto)"

%% %format `k` = "\rightsquigarrow"
%% %format k = "(\rightsquigarrow)"

\framet{Abstract algebra for functions}{
\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)

NOP
class Category k => ProductCat k where
  exl    ::  (a :* b) `k` a
  exr    ::  (a :* b) `k` b
  (&&&)  ::  (a `k` c)  -> (a `k` d)  -> (a `k` (c :* d))
\end{code}

\vspace{3ex}
Plus laws and classes for arithmetic etc.
}

%% class Category k => CoproductCat k where
%%   type Coprod k a b
%%   inl    ::  a `k` (Coprod k a b)
%%   inr    ::  b `k` (Coprod k a b)
%%   (|||)  ::  (a `k` c)  -> (b `k` c)  -> ((Coprod k a b) `k` c)

%format cosC = cos
%format sinC = sin
%format mulC = mul
%format addC = add
%format negateC = negate

%if False
\framet{Homomorphisms}{

A \emph{functor} |F| maps arrows from one category to another, preserving |Category| structure:
\begin{code}
F id == id

F (g . f) == F g . F f
\end{code}

\ 

\pause
A \emph{cartesian functor} |F| additionally preserves |Cartesian| structure:
\begin{code}
F exl  == exl

F exr  == exr

F (f &&& g)  == F f &&& F g
\end{code}
}
%endif

\framet{Automatic differentiation (specification)}{

\begin{code}
newtype D a b = D (a -> b :* (a :-* b))

adf :: (a -> b) -> D a b
adf f = D (f &&& der f)     -- not computable
\end{code}
\pause

% Specification: |D| is a cartesian category, and |adf| preserves structure, i.e.,
Specification: |adf| preserves |Category| and |Cartesian| structure:
{\setlength{\blanklineskip}{1ex}
\begin{minipage}[c]{0.49\textwidth} % \mathindent1em
\begin{code}
adf id == id

adf (g . f) == adf g . adf f
\end{code}
\end{minipage}
% \hspace{1ex}
\begin{minipage}[c]{0.49\textwidth} % \mathindent1em
\begin{code}
adf exl  == exl

adf exr  == exr

adf (f &&& g)  == adf f &&& adf g
\end{code}
\end{minipage}
}

%\pause
\emph{The game:} solve these equations for the RHS operations.
}

\framet{Automatic differentiation (solution)}{
\mathindent-1ex
\begin{code}
newtype D a b = D (a -> b :* (a :-* b))
\end{code}
%% \pause
\vspace{-4ex}
\begin{code}
linearD f = D (\ a -> (f a, f))

instance Category D where
  id = linearD id
  D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))

instance Cartesian D where
  exl  = linearD exl
  exr  = linearD exr
  D f &&& D g = D (\ a -> let { (b,f') = f a ; (c,g') = g a } in ((b,c), f' &&& g'))

instance NumCat D where
  negateC = linearD negateC
  addC  = linearD addC
  mulC  = D (mulC &&& (\ (a,b) -> \ (da,db) -> b*da + a*db))
\end{code}
}

%if not icfp
\framet{Running examples}{
\begin{code}
sqr :: Num a => a -> a
sqr a = a * a

magSqr :: Num a => a :* a -> a
magSqr (a,b) = sqr a + sqr b

cosSinProd :: Floating a => a :* a -> a :* a
cosSinProd (x,y) = (cos z, sin z) where z = x * y
\end{code}

\pause
In categorical vocabulary:

\begin{code}
sqr = mulC . (id &&& id)

magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))

cosSinProd = (cosC &&& sinC) . mulC
\end{code}
}

\framet{Visualizing computations}{
\begin{code}
magSqr (a,b) = sqr a + sqr b
NOP
magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))
\end{code}
\vspace{-5ex}
\begin{center}\wpicture{4in}{magSqr}\end{center}

Auto-generated from Haskell code.
See \href{http://conal.net/papers/compiling-to-categories/}{\emph{Compiling to categories}}.
}

\framet{AD example}{
\vspace{4ex}
\begin{code}
sqr a = a * a

sqr = mulC . (id &&& id)
\end{code}
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{sqr}
\end{tcolorbox}
\end{textblock}
\pause

\vspace{0ex}
\begin{center}\wpicture{4.8in}{sqr-adf}\end{center}

%% \figoneW{0.51}{cosSinProd-ad}{|andDeriv cosSinProd|}{\incpic{cosSinProd-ad}}}
}

\framet{AD example}{
\vspace{8ex}
\begin{code}
magSqr (a,b) = sqr a + sqr b

magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))
\end{code}
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{magSqr}
\end{tcolorbox}
\end{textblock}
\pause

\vspace{-4ex}
\begin{center}\wpicture{4.5in}{magSqr-adf}\end{center}

}

\framet{AD example}{
\vspace{12ex}
\begin{code}
cosSinProd (x,y) = (cos z, sin z) where z = x * y

cosSinProd = (cosC &&& sinC) . mulC
\end{code}
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{cosSinProd}
\end{tcolorbox}
\end{textblock}
\pause

\begin{center}\wpicture{4.5in}{cosSinProd-adf}\end{center}
}
%endif

\framet{Generalizing AD}{
\mathindent-1ex
\begin{code}
newtype D a b = D (a -> b :* (a :-* b))
\end{code}
%% \pause
\vspace{-4ex}
\begin{code}
linearD f = D (\ a -> (f a, f))

instance Category D where
  id = linearD id
  D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))

instance Cartesian D where
  exl  = linearD exl
  exr  = linearD exr
  D f &&& D g = D (\ a -> let { (b,f') = f a ; (c,g') = g a } in ((b,c), f' &&& g'))
\end{code}

\vspace{1.5ex}
%% \pause

Each |D| operation just uses corresponding |(:-*)| operation.\\[2ex]
Generalize from |(:-*)| to other cartesian categories.\\[4.1ex]
}

%% %format GD = GAD

%format (GD (k)) = D"_{"k"}"
%% %format GD (k) a b = a "\leadsto_{"k"}" b

\framet{Generalized AD}{
\mathindent-1ex
\begin{code}
newtype GD k a b = D (a -> b :* (a `k` b))
\end{code}
%% \pause
\vspace{-4ex}
\begin{code}
linearD f f' = D (\ a -> (f a, f'))

instance Category k => Category (GD k) where
  id = linearD id id
  D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))

instance Cartesian k => Cartesian (GD k) where
  exl  = linearD exl exl
  exr  = linearD exr exr
  D f &&& D g = D (\ a -> let { (b,f') = f a ; (c,g') = g a } in ((b,c), f' &&& g'))
\end{code}
\vspace{-5ex}
\begin{code}
instance SPC ... => NumCat D where
  negateC = linearD negateC negateC
  addC  = linearD addC addC
  mulC  = ??
\end{code}
}

%format inlP = inl
%format inrP = inr
%format |||| = |||
%format ++++ = +++

\framet{Numeric operations}{
Specific to (linear) \emph{functions}:

>      mulC  = D (mulC &&& (\ (a,b) -> \ (da,db) -> b*da + a*db))

%if not icfp
\pause
%endif

Rephrase:

\begin{code}
scale :: Multiplicative a => a -> (a :-* a)
scale u = \ v -> u * v

(||||) :: (a :-* c) -> (b :-* c) -> ((a :* b) :-* c)
f |||| g = \ (a,b) -> f a ^+^ g b
\end{code}

Now

>      mulC  = D (mulC &&& (\ (a,b) -> scale b |||| scale a))

}

%if not icfp
\framet{Linear arrow (biproduct) vocabulary}{
\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => ProductCat k where
  exl    :: (a :* b) `k` a
  exr    :: (a :* b) `k` b
  (&&&)  :: (a `k` c) -> (a `k` d) -> (a `k` (c :* d))

class Category k => CoproductCat k where
  inl    :: a `k` (a :* b)
  inr    :: b `k` (a :* b)
  (|||)  :: (a `k` c) -> (b `k` c) -> ((a :* b) `k` c)

class ScalarCat k a where
  scale :: a -> (a `k` a)
\end{code}
}
%endif

%format Double = R

%format -+> = "\mathbin{\rightarrow^{\!\!+}\!}"
%% %format -+> = "\rightarrow_{\!\!+}"
%% %format -+> = "\overset{+}{\longrightarrow}"
%% %format -+> = "\overset{{}_{+}}{\longrightarrow}"
%% %format -+> = "\rightarrow\hspace{-3ex}^{\scriptscriptstyle +}\hspace{2ex}"
%% %format -+> = "\mathbin{\longrightarrow\hspace{-3ex}{+}\hspace{0.7ex}}"

%format inNew2

%format inAbst2 = inNew2
%format joinP = join
%format unjoinP = unjoin

\framet{Linear transformations as functions}{
\vspace{-1.2ex}
\begin{code}
newtype a -+> b = AddFun (a -> b)

instance Category (-+>) where
  id   = AddFun id
  (.)  = inNew2 (.)

instance ProductCat (-+>) where
  exl    = AddFun exl
  exr    = AddFun exr
  (&&&)  = inNew2 (&&&)

instance CoproductPCat (-+>) where
  inlP   = AddFun (,zeroV)
  inrP   = AddFun (zeroV,)
  (||||) = inNew2 (\ f g (x,y) -> f x ^+^ g y)

instance Multiplicative s => ScalarCat (-+>) s where
  scale s = AddFun (s NOP *)
\end{code}
}

%format toV = "\Varid{to}_V"
%format unV = "\Varid{un}_V"
%format (LC (s)) = M"_{"s"}"
%format V (s) = V"_{"s"}"
%format HasV (s) = HasV"_{"s"}"

\framet{Extracting a data representation}{
\begin{itemize}
\itemsep2ex
\parskip1ex
%if icfp
%% \itemsep1ex
%% \parskip0.75ex
%else
%endif
%if icfp
\item Finally, extract a matrix or gradient vector.
\item Very inefficient for gradient-based optimization!
\item Alternatively, represent as ``generalized matrices'' (|LC s a b|).\\
      (Solve more homomorphisms.)
%else
\item How to extract a matrix or gradient vector?
\item Sample over a domain \emph{basis} (rows of identity matrix).
\item For $n$-dimensional \emph{domain},
  \begin{itemize}\itemsep2ex
  \item Make $n$ passes.
  \item Each pass works on $n$-D sparse (``one-hot'') input.
  \item Very inefficient.
  \end{itemize}
\item For gradient-based optimization,
  \begin{itemize}\itemsep2ex
  \item High-dimensional domain.
  \item Very low-dimensional (1-D) codomain.
  \end{itemize}
%endif
\end{itemize}
}

%format applyL = applyM
%if not icfp
\framet{Generalized matrices}{\mathindent2ex
\vspace{-1.3ex}
\begin{code}
newtype LC s a b = L (V s b (V s a s))

NOP
applyL :: LC s a b -> (a -> b)
\end{code}
%if False
\vspace{-7ex}
\begin{code}
applyL as a = unV ((NOP <.> toV a) <$> as)

NOP
class HasV s a where
  type V s a :: * -> * -- |a| as container of |s| values
  toV  :: a -> V s a s
  unV  :: V s a s -> a
\end{code}
%else
\vspace{5ex}

%endif
Require |applyL| to preserve structure. Solve for methods.
}

\framet{Core vocabulary}{
Sufficient to build arbitrary ``matrices'':
\vspace{3ex}
\begin{code}
  scale  :: a -> (a `k` a)                              -- $1\times1$

  (|||)  :: (a `k` c) -> (b `k` c) -> ((a :* b) `k` c)  -- horizontal juxt

  (&&&)  :: (a `k` c) -> (a `k` d) -> (a `k` (c :* d))  -- vertical juxt
\end{code}

\vspace{3ex}
Types guarantee rectangularity.
}
%endif

\framet{Efficiency of composition}{
\begin{itemize}
%if icfp
\itemsep4ex
%else
\itemsep2ex \parskip0.5ex
%endif
\item
  Composition is associative.
%if not icfp
\item 
  Some associations are more efficient than others, so
  \begin{itemize}\itemsep2ex
  \item Associate optimally.
  \item Equivalent to \emph{matrix chain multiplication} --- $O(n \log n)$.
  \item Choice determined by \emph{types}, i.e., compile-time information.
  \end{itemize}
\pause
%endif
\item
  All right: ``forward mode AD'' (FAD).
\item
  All left: ``reverse mode AD'' (RAD).
\item
  RAD is \emph{much} better for gradient-based optimization.
\end{itemize}
}

%format --> = "\mapsto"
\framet{Left-associating composition (RAD)}{
CPS-like category:
\vspace{2ex}

\begin{itemize}\itemsep4ex
\item Represent |a `k` b| by |(b `k` r) -> (a `k` r)|.
\item Meaning:
  %% |ab --> (\ br -> br . ab)|.
  |f' --> (\ h -> h . f')|.
  %% |f' --> (. NOP f')|.
%if False
\item Results in left-composition.
\item Initialize with |id :: r `k` r|.
%endif
%% \item Corresponds to a categorical pullback.
\item Construct |h . der f a| directly, without |der f a|.\\
       %% Often eliminates large \& sparse matrices.
\end{itemize}
}

%% Doesn't type-check, because (->) is not in CoproductPCat.
%% See ConCat.Continuation

%format ContC (k) = Cont"_{"k"}"
%format (ContC (k) (r)) = Cont"_{"k"}^{"r"}"

%if True
\framet{Continuation category (specification)}{ %\mathindent0ex
\setlength{\blanklineskip}{1ex}
%% \vspace{-1.3ex}
%%  type Ok (ContC k r) = Ok k
\begin{code}
newtype ContC k r a b = Cont ((b `k` r) -> (a `k` r))

NOP
cont :: Category k => (a `k` b) -> ContC k r a b
cont f = Cont (. NOP f)
\end{code}

%if icfp
\vspace{10ex}
%else
\vspace{2ex}
%endif

% Spec: |ContC k r| is a cartesian category, and |cont| preserves structure.

Require |cont| to preserve structure. Solve for methods.

%if not icfp
\pause\vspace{3ex}

We'll use an isomorphism:
\begin{code}
join    :: Cocartesian  k => (c `k` a) :* (d `k` a) -> ((c :* d) `k` a)
unjoin  :: Cocartesian  k => ((c :* d) `k` a) -> (c `k` a) :* (d `k` a)

join (f,g)  = f ||| g
unjoin h    = (h . inl, h . inr)
\end{code}
%endif
}

\framet{Continuation category (solution)}{\mathindent0ex
\begin{code}
instance Category k => Category (ContC k r) where
  id = Cont id
  Cont g . Cont f = Cont (f . g)

instance ProductCat k => ProductCat (ContC k r) where
  exl  = Cont (join . inl)
  exr  = Cont (join . inr)
  (&&&) = inNew2 (\ f g -> (f ||| g) . unjoin)

instance CoproductCat k => CoproductCat (ContC k r) where
  inl  = Cont (exl . unjoin)
  inr  = Cont (exr . unjoin)
  (|||) = inNew2 (\ f g -> join . (f &&& g))

instance ScalarCat k a => ScalarCat (ContC k r) a where
   scale s = Cont (scale s)
\end{code}}
%endif

\framet{Reverse-mode AD without tears}{\mathindent2in
%if not icfp
\pause
%endif
\begin{code}
GD (ContC (LC s) r)
\end{code}
}

%if not icfp
\framet{Left-associating composition (RAD)}{
\begin{itemize}\itemsep2ex \parskip1ex
\item CPS-like category:
  \begin{itemize}\itemsep2ex
  \item Represent |a `k` b| by |(b `k` r) -> (a `k` r)|.
  \item Meaning:
    |f --> (. NOP f)|.
  \item Results in left-composition.
  \item Initialize with |id :: r `k` r|.
  %% \item Corresponds to a categorical pullback.
  \item Construct |h . der f a| directly, without |der f a|.\\
        %% Often eliminates large \& sparse matrices.
  \end{itemize}
\pitem We've seen this trick before:
  \begin{itemize}\itemsep2ex
  \item Transforming naive |reverse| from quadratic to linear.
  \item List generalizes to monoids, and monoids to categories.
  \end{itemize}
\end{itemize}
}

\framet{One of my favorite papers}{
  \begin{itemize}\itemsep3ex \parskip1ex
  \item \href{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.83.8567}{\emph{Continuation-Based Program Transformation Strategies}} \\ Mitch Wand, 1980, JACM.
  \item Introduce a continuation argument, e.g., |[a] -> [a]|.
  \item Notice the continuations that arise, e.g., |(++ NOP as)|.
  \pitem Find a \emph{data} representation, e.g., |as :: [a]|
  \item Identify associative operation that represents composition,\\
  e.g., |(++)| , since |(++ NOP bs) . (++ NOP as) == (++ NOP (as ++ bs))|.
  \end{itemize}
}
%endif

\framet{Duality}{
%\pause
\begin{itemize}
%if icfp
\itemsep4ex
%else
\itemsep3ex
%endif
\item Vector space dual: |u :-* s|, with |u| a vector space over |s|.
\item If |u| has finite dimension, then |u :-* s =~= u|.
%if not icfp
\item For |f :: u :-* s|, |f == dot v| for some |v :: u|.
\item Gradients are derivatives of functions with scalar codomain.
%endif
\item Represent |a :-* b| by |(b :-* s) -> (a :-* s)| by |b -> a|.
\item \emph{Ideal} for extracting gradient vector.
      Just apply to |1| (|id|).
%% \item Often don't need vector space; semi-module will do.
%% \item Semimodule suffices in place of vector space.
\end{itemize}
}


%format (DualC (k)) = Dual"_{"k"}"
%format unDot = dot"^{-1}"
\framet{Duality (specification)}{
\begin{code}
newtype DualC k a b = Dual (b `k` a)

asDual :: ContC k s a b -> DualC k a b
asDual (Cont f) = Dual (unDot . f . dot)
\end{code}
where
\begin{code}
dot    :: u -> (u :-* s)
unDot  :: (u :-* s) -> u
\end{code}
\vspace{2ex}

Require |asDual| to preserve structure. Solve for methods.
}

\framet{Duality (solution)}{
\begin{code}
newtype DualC k a b = Dual (b `k` a)

instance Category k => Category (DualC k) where
  id   = Dual id
  (.)  = inNew2 (flip (.))

instance CoproductCat k => ProductCat (DualC k) where
  exl    = Dual inlP
  exr    = Dual inrP
  (&&&)  = inNew2 (||||)

instance ProductCat k => CoproductPCat (DualC k) where
  inlP    = Dual exl
  inrP    = Dual exr
  (||||)  = inNew2 (&&&)

instance ScalarCat k s => ScalarCat (DualC k) s where
  scale s = Dual (scale s)
\end{code}
}

\framet{Backpropagation}{\mathindent2in
\pause
\begin{code}
GD (DualC (-+>))
\end{code}
}

%if not icfp

\framet{RAD example (dual function)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{add}
\end{tcolorbox}
\end{textblock}
\vspace{10ex}
\begin{center}\hspace{-5ex}\wpicture{4in}{add-adr}\end{center}
}
\framet{RAD example (dual vector)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{add}
\end{tcolorbox}
\end{textblock}
\vspace{12ex}
\begin{center}\hspace{-5ex}\wpicture{4in}{add-gradr}\end{center}
}

\framet{RAD example (dual function)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{dup}
\end{tcolorbox}
\end{textblock}
\vspace{13ex}
\begin{center}\hspace{-5ex}\wpicture{4.5in}{dup-adr}\end{center}
}
\framet{RAD example (vector)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{dup}
\end{tcolorbox}
\end{textblock}
\vspace{12ex}
\begin{center}\hspace{-5ex}\wpicture{2.5in}{dup-gradr}\end{center}
}

\framet{RAD example (dual function)}{
\begin{textblock}{130}[1,0](357,37)
\begin{tcolorbox}
\wpicture{1.5in}{fst}
\end{tcolorbox}
\end{textblock}
\vspace{8ex}
\begin{center}\hspace{-8ex}\wpicture{3.7in}{fst-adr}\end{center}
}
\framet{RAD example (dual vector)}{
\begin{textblock}{130}[1,0](357,37)
\begin{tcolorbox}
\wpicture{1.5in}{fst}
\end{tcolorbox}
\end{textblock}
\vspace{4ex}
\begin{center}\hspace{0ex}\wpicture{2.5in}{fst-gradr}\end{center}
}

\framet{RAD example (dual function)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{sqr}
\end{tcolorbox}
\end{textblock}
\vspace{12ex}
\begin{center}\hspace{0ex}\wpicture{4.5in}{sqr-adr}\end{center}
}
\framet{RAD example (dual vector)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{sqr}
\end{tcolorbox}
\end{textblock}
\vspace{11ex}
\begin{center}\hspace{-2ex}\wpicture{4in}{sqr-gradr}\end{center}
}

\framet{RAD example (dual function)}{
\vspace{2ex}
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{magSqr}
\end{tcolorbox}
\end{textblock}
\vspace{4ex}
\begin{center}\hspace{-2ex}\wpicture{4.5in}{magSqr-adr}\end{center}
}
\framet{RAD example (dual vector)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{magSqr}
\end{tcolorbox}
\end{textblock}
\vspace{7ex}
\begin{center}\hspace{-4ex}\wpicture{3.8in}{magSqr-gradr}\end{center}
}

\framet{RAD example (dual function)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{cosSinProd}
\end{tcolorbox}
\end{textblock}
\vspace{10ex}
\begin{center}\hspace{-0.5ex}\wpicture{4.8in}{cosSinProd-adr}\end{center}
}
\framet{RAD example (matrix)}{
\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{cosSinProd}
\end{tcolorbox}
\end{textblock}
\vspace{10.5ex}
\begin{center}\hspace{-2ex}\wpicture{4.4in}{cosSinProd-adrl}\end{center}
}

\framet{Incremental evaluation}{
\pause
\begin{textblock}{140}[1,0](357,37)
\begin{tcolorbox}
\wpicture{1.7in}{magSqr}
\end{tcolorbox}
\end{textblock}
\vspace{9.5ex}
\begin{center}\hspace{-0.5ex}\wpicture{4.8in}{magSqr-andInc}\end{center}
}

%endif

%format ### = ||||

\framet{Conclusions}{
\begin{itemize}\itemsep2.5ex
\item Simple AD algorithm, specializing to forward, reverse, mixed.
\item No graphs, tapes, tags, partial derivatives, or mutation.
\item Parallel-friendly and low memory use.
\item Calculated
  % rigorously
  from simple, regular
  algebra problems.
  %% homomorphic specifications.
%% \item One rule per combining form: |(.)|, |(&&&)|, |(###)|.
%% \item RAD as simple as FAD but very efficient for gradient problems.
%% \item Reverse mode via simple, general constructions.
\item Generalizes to derivative categories other than linear maps.
\item Differentiate regular Haskell code (via plugin).
%if icfp
\item Paper: pictures, proofs, incremental computation.
%else
\item More details in my \href{http://conal.net/papers/essence-of-ad/}{ICFP 2018 paper}.
%endif
\end{itemize}
}

\framet{Reflections: recipe for success}{

\pause
Key principles:

\begin{itemize}\itemsep2.5ex
\item
  Capture main concepts as first-class values.
\item
  Focus on abstract notions, not specific representations.
\item
  Calculate efficient implementation from simple specification.
\end{itemize}

Not previously applied to AD (afaik).

\pause\vspace{4ex}

\emph{Quandary:} Most programming languages poor for function-like things.

\pause\vspace{2ex}

\emph{Solution:} \emph{\href{http://conal.net/papers/compiling-to-categories}{Compiling to categories}}.

}

\framet{Symbolic vs automatic differentiation}{
Often described as opposing techniques:
\begin{itemize}\itemsep2ex
\item \emph{Symbolic}:
  \begin{itemize}\itemsep1.5ex
  \item Apply differentiation rules symbolically.
  \item Can duplicate much work.
  \item Needs algebraic manipulation.
\end{itemize}
\item \emph{Automatic}:
\begin{itemize}\itemsep1.5ex
  \item FAD: easy to implement but often inefficient.
  \item RAD: efficient but tricky to implement.
\end{itemize}
\end{itemize}
\pause
\vspace{2ex}
My view: \emph{AD is SD done by a compiler.}\\[2ex]
Compilers already work symbolically and preserve sharing.
}

\end{document}
