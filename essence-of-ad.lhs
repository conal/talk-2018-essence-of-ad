%% -*- latex -*-

% Presentation
%\documentclass[aspectratio=1610]{beamer} % Macbook Pro screen 16:10
\documentclass{beamer} % default aspect ratio 4:3
%% \documentclass[handout]{beamer}

% \setbeameroption{show notes} % un-comment to see the notes

\input{macros}

%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include formatting.fmt

\title[]{The simple essence of automatic differentiation}
\date{January 2018}
\institute[]{Target}

\setlength{\itemsep}{2ex}
\setlength{\parskip}{1ex}
\setlength{\blanklineskip}{1.5ex}
\setlength\mathindent{4ex}

\nc\wow\emph

\usepackage{scalerel}

\begin{document}

% \large

\frame{\titlepage}
\title{Essence of automatic differentiation}
% \title{Simple essence of AD}
% \institute{Target}
\date{Jan 2018}

\framet{What's a derivative?}{

For scalar domain: $$|der f x = lim(epsilon -> 0)(frac(f (x+epsilon) - f x) epsilon)|$$

\ 
\pause
Redefine: unique scalar $s$ such that
 $$ |lim(epsilon -> 0)(frac(f (x+epsilon) - f x) epsilon) - s == 0| $$

\pause
Equivalently,
 $$ |lim(epsilon -> 0)(frac(f (x+epsilon) - (f x + s *^ epsilon)) epsilon) == 0| $$

}

\framet{What's a derivative?}{
For scalar domain:
 $$ |lim(epsilon -> 0)(frac(f (x+epsilon) - (f x + s *^ epsilon)) epsilon) == 0| $$

\pause

Now generalize: unique \wow{linear map} $T$ such that

$$|lim(epsilon -> 0)(frac(abs (f (x+epsilon) - (f x + T epsilon)))(abs epsilon)) == 0|$$

\pause\vspace{3ex}

\wow{Derivatives are linear maps:}

> der :: (a -> b) -> (a -> (a :-* b))

%% Captures all ``partial derivatives'' for all dimensions.

See \emph{Calculus on Manifolds} by Michael Spivak.

}

\framet{Composition}{

Sequential and parallel:

\begin{code}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(g . f) a = g (f a)
NOP
(&&&) :: (a -> c) -> (a -> d) -> (a -> c :* d)
(f &&& g) a = (f a, g a)
\end{code}

\pause
Differentiation rules:
\begin{code}
der (g . f) a == der g (f a) . der f a    -- ``chain rule''
NOP
der (g &&& f) a == der g a &&& der f a
\end{code}
}

\framet{Compositionality}{

Chain rule:

> der (g . f) a == der g (f a) . der f a     -- non-compositional

\ 

To fix, combine primal (regular result) with derivative:
\begin{code}
andDer :: (a -> b) -> (a -> (b :* (a :-* b)))
andDer f = f &&& der f     -- specification
\end{code}

Often much work in common to |f| and |der f|.
}

\framet{Linear functions}{

Linear functions are their own perfect linear approximations.

\begin{code}
der id   a  =  id
der fst  a  =  fst
der snd  a  =  snd
            ...
\end{code}

For linear functions |f|,

> andDer f a = (f a, f)

%% > andDer f = f &&& const f

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

\framet{Abstract algebra for functions}{

\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => ProductCat k where
  type Prod k a b
  exl    ::  (Prod k a b) `k` a
  exr    ::  (Prod k a b) `k` b
  (&&&)  ::  (a `k` c)  -> (a `k` d)  -> (a `k` (Prod k c d))

class Category k => CoproductCat k where
  type Coprod k a b
  inl    ::  a `k` (Coprod k a b)
  inr    ::  b `k` (Coprod k a b)
  (|||)  ::  (a `k` c)  -> (b `k` c)  -> ((Coprod k a b) `k` c)
\end{code}

\vspace{-1ex}
Plus laws and classes for arithmetic etc.

}

\framet{Simple automatic differentiation}{
\mathindent-1ex
\begin{code}
data D a b = D (a -> b :* (a :-* b))
\end{code}
\pause
\vspace{-4ex}
\begin{code}
linearD f = D (f &&& const f)

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
  mulC  = D (mulC &&& \ (a,b) -> (\ (da,db) -> b*da + a*db))
\end{code}
}

\framet{Running examples}{

\begin{code}
sqr :: Num a => a -> a
sqr a = a * a

magSqr :: Num a => a :* a -> a
magSqr (a,b) = sqr a + sqr b

cosSinProd :: Floating a => a :* a -> a :* a
cosSinProd (x,y) = (cos z, sin z) where z = x * y
\end{code}

In categorical vocabulary:

\begin{code}
sqr = mulC . (id &&& id)

magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))

cosSinProd = (cosC &&& sinC) . mulC
\end{code}
}

\framet{Visualizing computations}{

> magSqr (a,b) = sqr a + sqr b
> NOP
> magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))

\vspace{-5ex}
\begin{center}\wpicture{4in}{magSqr}\end{center}

Auto-generated from Haskell code.
See \href{http://conal.net/papers/compiling-to-categories/}{\emph{Compiling to categories}}.
}

\framet{AD example}{
\vspace{4ex}

> sqr a = a * a
>
> sqr = mulC . (id &&& id)

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

> magSqr (a,b) = sqr a + sqr b
>
> magSqr = addC . (mulC . (exl &&& exl) &&& mulC . (exr &&& exr))

\begin{textblock}{160}[1,0](357,37)
\begin{tcolorbox}
\wpicture{2in}{magSqr}
\end{tcolorbox}
\end{textblock}
\pause

\vspace{-4ex}
\begin{center}\wpicture{4.5in}{magSqr-adf}\end{center}

%% \figoneW{0.51}{cosSinProd-ad}{|andDeriv cosSinProd|}{\incpic{cosSinProd-ad}}}
}

\framet{Generalizing AD}{
\mathindent-1ex
\begin{code}
data D a b = D (a -> b :* (a :-* b))
\end{code}
\pause
\vspace{-6ex}
\begin{code}
linearD f = D (f &&& const f)

instance Category D where
  id = linearD id
  D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))

instance Cartesian D where
  exl  = linearD exl
  exr  = linearD exr
  D f &&& D g = D (\ a -> let { (b,f') = f a ; (c,g') = g a } in ((b,c), f' &&& g'))
\end{code}

\vspace{0.5ex}
\pause

Each |D| operation just uses corresponding |(:-*)| operation.

Generalize from |(:-*)| to other cartesian categories.
}

\framet{Generalized AD}{
\mathindent-1ex
\begin{code}
data GD k a b = D (a -> b :* (a `k` b))

instance Category k => Category (GD k) where
  id = linearD id
  D g . D f = D (\ a -> let { (b,f') = f a ; (c,g') = g b } in (c, g' . f'))

instance Cartesian k => Cartesian (GD k) where
  exl  = linearD exl
  exr  = linearD exr
  D f &&& D g = D (\ a -> let { (b,f') = f a ; (c,g') = g a } in ((b,c), f' &&& g'))
\end{code}

}

\framet{Numeric operations}{

Specific to (linear) \emph{functions}:

>      mulC  = D (mulC &&& \ (a,b) -> (\ (da,db) -> b*da + a*db))

\pause

Rephrase:

\begin{code}
scale :: Multiplicative a => a -> (a -> a)
scale u = \ v -> u * v

(||||) :: Additive c => (a -> c) -> (b -> c) -> ((a :* b) -> c)
f |||| g = \ (a,b) -> f a ^+^ g b
\end{code}

Now

>      mulC  = D (mulC &&& (scale b |||| scale a))

}

\framet{Linear arrow vocabulary}{
\begin{code}
class Category k where
  id   :: a `k` a
  (.)  :: (b `k` c) -> (a `k` b) -> (a `k` c)

class Category k => ProductCat k where
  exl    :: (a :* b) `k` a
  exr    :: (a :* b) `k` b
  (&&&)  :: (a `k` c) -> (a `k` d) -> (a `k` (c :* d))

class Category k => CoproductCat k where
  inl    :: Additive b  => a `k` (a :* b)
  inr    :: Additive a  => b `k` (a :* b)
  (|||)  :: Additive c  => (a `k` c) -> (b `k` c) -> ((a :* b) `k` c)

class ScalarCat k a where
  scale :: a -> (a `k` a)
\end{code}
}

\framet{Generalized matrix construction}{

\begin{code}
  scale  :: a -> (a `k` a)                              -- $1\times1$

  (|||)  :: (a `k` c) -> (b `k` c) -> ((a :* b) `k` c)  -- horizontal juxt

  (&&&)  :: (a `k` c) -> (a `k` d) -> (a `k` (c :* d))  -- vertical juxt
\end{code}

\vspace{2ex}
Sufficient to build arbitrary matrices.

\vspace{4ex}
Types guarantee rectangularity.

}

%format Double = R

\framet{A ``matrix'' representation}{\mathindent2ex
\begin{code}
newtype L s a b = L (V s b (V s a s))

class HasV s a where
  type V s a :: * -> * -- Free vector space as representable functor
  toV  :: a -> V s a s
  unV  :: V s a s -> a

instance HasV Double Double where
  type V Double Double = Par1
  toV  = Par1
  unV  = unPar1

instance (HasV s a, HasV s b) => HasV s (a :* b) where
  type V s (a :* b) = V s a :*: V s b
  toV (a , b)    = toV a :*: toV b
  unV (f :*: g)  = (unV f,unV g)
\end{code}
}

\framet{Efficiency of composition}{
\begin{itemize}\itemsep2ex
\item
  Some orders of composition are more efficient than others.
\item
  Fortunately, arrow composition is associative:
  \begin{itemize}\itemsep2ex
  \item Associate optimally.
  \item Equivalent to \emph{matrix chain multiplication} --- $O(n \log n)$.
  \item Choice determined by \emph{types}, i.e., compile-time information.
  \end{itemize}
\item
  All right composition gives ``forward mode AD'' (FAD).\\
  Good for scalar domains.
\item
  All left composition gives ``reverse mode AD'' (RAD).\\
  Good for scalar codomains.
\item
  RAD is a much better choice for gradient-based optimization.
\end{itemize}
}

%format --> = "\mapsto"
\framet{Left-associating composition (RAD)}{
\begin{itemize}\itemsep2ex \parskip1ex
\item CPS-like category:
  \begin{itemize}\itemsep2ex
  \item Represent |a `k` b| by |(b `k` r) -> (a `k` r)|.
  \item Meaning: |ab --> \ br -> br . ab|.
  \item Results in left-composition.
  \item Corresponds to a categorical pullback.
  \item Duality/transposition in linear algebra.
  \end{itemize}
\item We've seen this trick before:
  \begin{itemize}\itemsep2ex
  \item Transforming naive |reverse| from quadratic to linear.
  \item Lists generalize to monoids, and monoids to categories.
  \end{itemize}
\end{itemize}
}

\framet{One of my favorite papers}{
  \begin{itemize}\itemsep2ex
  \item \href{http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.83.8567}{\emph{Continuation-Based Program Transformation Strategies}} \\ Mitch Wand, 1980, JACM.
  \item Introduce a continuation argument, e.g., |[a] -> [a]|.
  \item Notice the continuations that arise, e.g., |(++ as)|.
  \item Find a \emph{data} representation, e.g., |as :: [a]|
  \item Identify operation that represents composition, e.g., |(++)|
  \item Representation will have corresponding associativity property.
  \end{itemize}
}


%% %format dot u v = u <.> v
%format dot u v = "\langle" u "," v "\rangle"

\framet{Eliminating matrices}{
\begin{itemize}\itemsep2ex
\item Vector space dual: |U :-* r|, with |U| a vector space over |r|.
\item If |U| has finite dimension, then |U :-* r =~= U|.
\item If |f :: U :-* r|, then |f u == dot v u| for some |v|.
\item Gradients are derivatives of functions with scalar codomain.
\item Represent |a `k` b| by |(b `k` r) -> (a `k` r)| by |b -> a|.
\item %% Construct |br . der ab a| without building |der ab a|.
 Construct |dot v . der f a| directly, without |dot v| or |der f a|.
\item Eliminates matrices (often sparse/expensive).
\end{itemize}
}

%format inDual2

\framet{Dual categories}{
\begin{code}
data Dual k a b = Dual (b `k` a)

instance Category k => Category (Dual k) where
  id   = Dual id
  (.)  = inDual2 (flip (.))

instance CoproductCat k => ProductCat (Dual k) where
  exl   = Dual inlD
  exr   = Dual inrD
  (&&&) = inDual2 (|||)

instance ProductCat k => CoproductPCat (Dual k) where
  inl   = Dual exl
  inr   = Dual exr
  (|||) = inDual2 (&&&)

instance ScalarCat k s => ScalarCat (Dual k) s where
  scale s = Dual (scale s)
\end{code}
}

\framet{Reverse-mode AD without tears}{

> type RAD = GD (Dual AdditiveFun)

}

\end{document}
