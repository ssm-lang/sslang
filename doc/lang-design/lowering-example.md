# Operation Be-The-Compiler: Lowering Example

## Background Info
**Compiler Pipeline**
`Scanner --> Parser --> Lowering --> Type Inference --> Optimization --> Code Generation --> C code`

**Lowering Stage**
The ouput of the parser gives us a representation of our input sslang program as a tree. Nodes in the tree represents parts of the input program.
The "lowering" pass implemented in `LowerAst.hs` transforms every AST node into an equivalient IR node.
After performing lowering, we are still representing our input sslang program as a tree in the compiler. The difference is that after lowering, there are fewer kinds of nodes in our tree, and each node is augmented with space for a type, t.
```
// an AST Expr Node     ----- LowerAst.hs ---->     // an IR Expr Node

data Expr                                            data Expr t              
  = Id Identifier                                      = Var VarId t
  | Lit Literal                                        | Data DConId t
  | Apply Expr Expr                                    | Lit Literal t
  | Lambda [Pat] Expr                                  | App (Expr t) (Expr t) t
  | OpRegion Expr OpRegion                             | Let [(Binder, Expr t)] (Expr t) t
  | NoExpr                                             | Lambda Binder (Expr t) t
  | Let [Definition] Expr                              | Match (Expr t) [(Alt, Expr t)] t
  | While Expr Expr                                    | Prim Primitive [Expr t] t
  | Loop Expr                                          deriving (Eq, Show, Typeable, Data, Functor, Foldable, Traversable)
  | Par [Expr]
  | IfElse Expr Expr Expr
  | After Expr Expr Expr
  | Assign Expr Expr
  | Constraint Expr TypAnn
  | Wait [Expr]
  | Seq Expr Expr
  | Break
  | Match Expr [(Pat, Expr)]
  | CQuote String
  | CCall Identifier [Expr]
  | ListExpr [Expr]
  deriving (Eq, Show)

-- | A literal                                       -- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@.
data Literal                                         type Binder = Maybe VarId
  = LitInt Integer
  | LitString String
  | LitRat Rational
  | LitChar Char
  | LitEvent
  deriving (Eq, Show)
```
The function `lowerExpr` inside `LowerAst.hs` has the type signature
`lowerExpr :: A.Expr -> Compiler.Pass (I.Expr I.Annotations)`
which means it transforms something of type `A.Expr` into a monadic `I.Expr t`. `Compiler.Pass` is the monad that wraps most all our compiler computations on/transformations of our representation of a sslang program.
## Example: tree.ssl
```
type Tree 
  TwoKids Int Tree Tree
  OneKid Int Tree
  Leaf

main cin cout = 
  let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
  ()
```
```
This tree x looks like...

                 5
                / \
               4   6
              /   / \
             *   *   *
```
**1) What does x look like as an AST node?**
*Answer*
```
let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
()
```
 as an AST node looks like
```
Let [DefPat (PatId "x") (...)] ()
where ... = (App (App (Id "TwoKids") (Lit 5)) (App (App (App (Id "OneKid") (Lit 4)) (Id "Leaf")) (App (App (App (Id "TwoKids") (Lit 6)) (Id "Leaf")) (Id "Leaf")))
```

**2) What should x look like as an IR node?**
*Answer*
```
let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
()
```
as an IR  node looks like
```
Let [(Just "x", ...)] (() t)
where ... = (App (App (Data "TwoKids" t) (Lit 5 t) t) (App (App (App (Data "OneKid" t) (Lit 4 t) t) (Data "Leaf" t) t) (App (App (App (Data "TwoKids" t) (Lit 6 t) t) (Data "Leaf" t) t) (Data "Leaf" t) t) t) t)
        t = (Annotation [])
```
  
## Solution Walk-Through
 **1) What does x look like as an AST node?**
 I'm going to ignore `main` which is in an AST node somewhere that eventually encloses x,  and am just going to focus on the lines 
 ```
let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
()
```
 We see that x is enclosed inside a let-expression node of the form
```
Let [Definition] Expr -- definition of Let instance of A.Expr node
```
The `Expr` part of the let form will be `()`, and the Definition part of let form will contain `x = ...`, where `...` abbreviates `(TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))` for now.

 Some helpful background info from `Ast.hs`:
 ```
 -- | A value definition.
data Definition
  = DefFn Identifier [Pat] TypFn Expr
  | DefPat Pat Expr
  deriving (Eq, Show)

-- | A pattern appearing on the LHS of a definition or match arm
data Pat
  = PatWildcard           -- ^ Match anything, i.e., @_@
  | PatId Identifier      -- ^ Variable or data constructor, e.g., @v@ or @Some@
  | PatLit Literal        -- ^ Literal match, e.g., @1@
  | PatAs Identifier Pat  -- ^ Pattern alias, e.g., @a \@ <pat>@
  | PatTup [Pat]          -- ^ Match on a tuple, e.g., @(<pat>, <pat>)@
  | PatApp [Pat]          -- ^ Match on multiple patterns, e.g., @Some a@
  | PatAnn Typ Pat        -- ^ Match with type annotation, e.g., @<pat>: Type@
  deriving (Eq, Show)
  ```

Based on these definitions,
```
let x = ...
()
```
 should have a form like this:
 ```
Let [Definition] Expr
         |         |
         |         v         
         |        NoExpr, or ()
         v 
       DefPat Pat Expr
               |   |
               |   |
               |   v
               |   App Expr Expr  
               v 
              PatId Identifier
```
Filling in the pieces, we have
```
Let [DefPat (PatId "x") (...)] ()
```
Now let's consider the stuff inside `DefPat` abbreviated by `...`,
```
(TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
```
I know this should be a nested application of data constructors...
```
(App (App TwoKids 5) (App (App (App OneKid 4) Leaf) (App (App (App TwoKids 6) Leaf) Leaf)))
```
`TwoKids` is really shorthand for the AST node `(Id "TwoKids")`.
`4` is really shorthand for the AST node `(Lit 4)`.
Getting rid of all of the shorthand notation, we have
```
(App (App (Id "TwoKids") (Lit 5)) (App (App (App (Id "OneKid") (Lit 4)) (Id "Leaf")) (App (App (App (Id "TwoKids") (Lit 6)) (Id "Leaf")) (Id "Leaf")))
```
As a tree this looks like
```
                            (App Expr Expr)
                                 /       \
                                /         \
                               /           \
                              /             \
                             /               \
                            /                 \
                           /                   \
                          /                     \
                         /                       \
                        /                         \
                       /                           \
                      /                             \
                     /                               \
                    /                                 \
                 (App Expr Expr)                     (App Expr Expr)
                      /      \                            /       \
                     /        \                          /         \
                    /          \                        /           \
                   /            \                      /             \
                  /              \                    /               \
                (Id "TwoKids")  (Lit 5)              /                 \
                                                    /                   \
                                                   /                     \
                                                  /                       \
                                                 /                         \
                                                /                           \
                                               /                             \
                                              /                               \
                                           (App Expr Expr)                (App Expr Expr)
                                                 /      \                      /      \                  
                                                /        \                    /        \
                                               /          \                  /          \
                                              /            \                /            \
                                             /              \              /              \
                                          (App Expr Expr)  (Id "leaf")    /                \
                                               /      \                  /                  \
                                              /        \               (App Expr Expr)      (Id "Leaf")
                                             /          \                   /       \
                                            /            \                 /         \
                                           /              \               /           \
                                        (Id "OneKid")    (Lit 4)         /             \
                                                                        /               \
                                                                     (App Expr Expr)    (Id "Leaf")
                                                                          /       \
                                                                         /         \
                                                                        /           \
                                                                       /             \
                                                                    (Id "TwoKids")   (Lit 6)
```
**2) What should x look like as an IR node?**
Recall that
 ```
let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))
()
```
as an AST node looks like
```
Let [DefPat (PatId "x") (...)] ()
where ... = (App (App (Id "TwoKids") (Lit 5)) (App (App (App (Id "OneKid") (Lit 4)) (Id "Leaf")) (App (App (App (Id "TwoKids") (Lit 6)) (Id "Leaf")) (Id "Leaf")))
```
**I need to convert every AST node into an IR node.**
```
Let [Definition] Expr  --- needs to turn into ---> Let [(Binder, Expr t)] (Expr t) t
     Definition        --- needs to turn into --->      (Binder, Expr t)
                 Expr  --- needs to turn into --->                        (Expr t)
```

Some helpful background info from `IR.hs`:
```
-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@.
type Binder = Maybe VarId
```
Based on the definition of `Binder`, we turn our instance of 
```
Definition DefPat (PatId "x") (...)
```
into a instance of a tuple of 
```
(Maybe VarId, Expr t)
```
 which looks like
```
(Just "x", ...)
```
Then the complete Let IR node looks like
```
Let [(Just "x", ...)] (() t)   -- ignore the t for now
```
Now let's consider the stuff in the RHS of the tuple (abbreviated with ... above),
```
(App (App (Id "TwoKids") (Lit 5)) (App (App (App (Id "OneKid") (Lit 4)) (Id "Leaf")) (App (App (App (Id "TwoKids") (Lit 6)) (Id "Leaf")) (Id "Leaf")))
```
We need to turn every AST node into an IR node.
```
// AST node   ------->    // IR node
Id Identifier             Data DConId t
Lit Literal               Lit Literal t
App Expr Expr             App (Expr t) (Expr t) t
```
Let's ignore  the `t` in the IR nodes for a moment. Then we have
```
(App (App (Data "TwoKids") (Lit 5)) (App (App (App (Data "OneKid") (Lit 4)) (Data "Leaf")) (App (App (App (Data "TwoKids") (Lit 6)) (Data "Leaf")) (Data "Leaf"))))
```
As a tree this looks like
```
                            (App Expr Expr)
                                 /       \
                                /         \
                               /           \
                              /             \
                             /               \
                            /                 \
                           /                   \
                          /                     \
                         /                       \
                        /                         \
                       /                           \
                      /                             \
                     /                               \
                    /                                 \
                 (App Expr Expr)                     (App Expr Expr)
                      /      \                            /       \
                     /        \                          /         \
                    /          \                        /           \
                   /            \                      /             \
                  /              \                    /               \
                (Data "TwoKids")  (Lit 5)            /                 \
                                                    /                   \
                                                   /                     \
                                                  /                       \
                                                 /                         \
                                                /                           \
                                               /                             \
                                              /                               \
                                           (App Expr Expr)                (App Expr Expr)
                                                 /      \                      /      \                  
                                                /        \                    /        \
                                               /          \                  /          \
                                              /            \                /            \
                                             /              \              /              \
                                         (App Expr Expr)  (Data "Leaf")   /                \
                                               /      \                  /                  \
                                              /        \               (App Expr Expr)      (Data "Leaf")
                                             /          \                   /       \
                                            /            \                 /         \
                                           /              \               /           \
                                        (Data "OneKid")  (Lit 4)         /             \
                                                                        /               \
                                                                     (App Expr Expr)    (Data "Leaf")
                                                                          /       \
                                                                         /         \
                                                                        /           \
                                                                       /             \
                                                                  (Data "TwoKids")   (Lit 6)
```
Now Recall:

    lowerExpr :: A.Expr -> Compiler.Pass (I.Expr I.Annotations)

`I.Expr t` is a *functor* and its parameter `t` is of type `I.Annotations`.
Some helpful background  info from IR.hs:
```
-- | Expressions are annotated with a (potentially empty) list of 'Annotation'.
newtype Annotations = Annotations [Annotation]
  deriving (Eq, Show, Typeable, Data, Semigroup, Monoid)

-- | An annotation records the annotated portion of a pattern.
data Annotation
  = AnnType Type                        -- ^ A basic 'Type' annotation
  | AnnDCon DConId [Annotation]         -- ^ Annotations collected from patterns
  | AnnArrows [Annotation] Annotation   -- ^ Annotations collected from fun args
  deriving (Eq, Show, Typeable, Data)
```
Notice `Annotations` is simply a list of `Annotation`, so we can fill in `t` with `Annotations []` and leave the rest to the future type checking pass. Adding `(Annotations [])` to each expression node would make our tree and parenthetical expression representation of the IR node very ugly. So let's just add `t`'s to each node and say that `t = (Annotations [])`.
So `let x = (TwoKids 5 (OneKid 4 Leaf) (TwoKids 6 Leaf Leaf))` as an IR looks like
```
Let [(Just "x", ...)] (() t)
where ... = (App (App (Data "TwoKids" t) (Lit 5 t) t) (App (App (App (Data "OneKid" t) (Lit 4 t) t) (Data "Leaf" t) t) (App (App (App (Data "TwoKids" t) (Lit 6 t) t) (Data "Leaf" t) t) (Data "Leaf" t) t) t) t)
        t = (Annotation [])
```
which as a tree looks like
```
                            (App (Expr t) (Expr t) t)
                                 /             \
                                /               \
                               /                 \
                              /                   \
                             /                     \
                            /                       \
                           /                         \
                          /                           \
                         /                             \
                        /                               \
                       /                                 \
                      /                                   \
                     /                                     \
                    /                                       \
                 (App (Expr t) (Expr t) t)                 (App (Expr t) (Expr t) t) 
                      /      \                                  /            \
                     /        \                                /              \
                    /          \                              /                \
                   /            \                            /                  \
                  /              \                          /                    \
                (Data "TwoKids" t)  (Lit 5 t)              /                      \
                                                          /                        \
                                                         /                          \
                                                        /                            \
                                                       /                              \
                                                      /                                \
                                                     /                                  \
                                                    /                                    \
                                                 (App (Expr t) (Expr t) t)                (App (Expr t) (Expr t) t)
                                                      /             \                           /           \                  
                                                     /               \                         /             \
                                                    /                 \                       /               \
                                                   /                   \                     /                 \
                                                  /                     \                   /                   \
                                              (App (Expr t) (Expr t) t)  (Data "Leaf" t)   /                     \
                                                    /         \                           /                       \
                                                   /           \                      (App (Expr t) (Expr t) t)   (Data "Leaf" t)
                                                  /             \                           /            \
                                                 /               \                         /              \
                                                /                 \                       /                \
                                             (Data "OneKid" t)   (Lit 4 t)               /                  \
                                                                                        /                    \
                                                                                       /                      \
                                                                                      /                        \
                                                                                     /                          \
                                                                                    /                            \
                                                                                 (App (Expr t) (Expr t) t)      (Data "Leaf" t)
                                                                                        /         \          
                                                                                       /           \
                                                                                      /             \
                                                                                     /               \
                                                                                 (Data "TwoKids" t)  (Lit 6 t)
```