Core is a simple functional language 
which is easy to implement but allows for
more advanced features to be compiled to it
from a higher level language.
E.g. Haskell eventually compiles to Core.

All implementations of Core will take a simple
program and execute it.


This document holds an intro to Core
, its syntax & a small prelude for it
along with the driver for further implementations.

Other documents have the standard Core datatypes
, a pretty printer for the language
and a parser.

> import Control.Monad (void)

> import Datatypes
> import PrettyPrint
> import Parser

The G-machine, the TIM machine and the parallel G-machine
will be used to process the parser's results.




Introduction to Core:
here is a simple Core program that evaluates to 42

  main = double 21
  double x = x + x

A core program is made up of supercombinator definitions
, including "main" which is executed to run the program.

Not all supercombinators take arguments
, main, for example. These are called 
constant applicative forms or CAFs
and often require special treatment in implementation.





Supercombinators can have local definitions 
using keyword "let"
    
    
        main = quadruple 20 ;
        quadruple x = let twice_x = x + x
                      in twice_x + twice_x
    
    In this case, only two additions will be performed.
    
    "let" and "letrec" (recursively defined locals)
    are both expressions:
    
    quad_plus_one = 1 + (let tx = x + x in tx + tx)

Lambda abstractions 
    
    It is convenient to denote functions anonymously 
    , e.g.
    double_list xs = map (\x. 2*x) xs
    
    Lambda lifting is when you turn all lambdas into top-level
    supercombinator definitions.
    This is discussed in ch6.

Algebraic/structured datatypes

    data Colour = Red | Green | Blue
    
    data ComplexI = Rect Int Int | Polar Int Int 
    
    Tree a = Leaf a | Branch (Tree a) (Tree a)
    
    
    Tree, ComplexI and Colour are types.
    To build them, use the constructors:
    
    Green
    Rect 10 2
    Branch (Leaf 1) (Leaf 50)

They are taken apart using pattern matching:

    isRed Red   = True
    isRed Green = False
    isRed Blue  = False
    
    depth (Leaf n) = 0
    depth (Branch t1 t2) = 1 + max (depth t1) (depth t2)
    
    
    Lists and booleans are just special datatypes too.
    
    data List a = Nil | Cons a (List a)
    
    
    To represent these in Core, we provide a 
    SINGLE
    family of constructors:
    
    Pack {tag, arity}
    
    where tag indicates the constructor used and
    arity is the # of arguments it takes.
    
    Red = Pack {1,0}
    Green = Pack {2,0}
    Blue = Pack {3,0}
    
    Leaf = Pack{1,1}
    Branch = Pack {2,2}
    
    
    Branch (Leaf 1111) (Leaf 9876) 
    = Pack{2,2} (Pack{1,1} 1111) (Pack{1,1} 9876)
    
    Note that in well typed programs
    , we can restart the tag #s from 1
    because constructors will never be confused for other types.
    
case expressions
    All complex forms of pattern matching are 
    banned in Core.
    Only "case" expressions are used.
    
    isRed c = case c of
        <1> -> True  ;
        <2> -> False ;
        <3> -> False ;
    
    The tags can take arguments:
    
    depth t = case t of
        <1> n -> 0 ;
        <2> t1 t2 -> max (depth t1) (depth t2)

Also see the example programs...



Core syntax:
    A table of infix ops and info follows:
    precedence, associativity, op
    
    6, left,  application
    5, right, *
     , nope , /
    4, right, +
     , nope,  -
    3, nooo,  == ~= > >= < <=
    2, right, &
    1, right, |
    
    "negate" and "not" are not binary and
    thus provided as functions.
    
    no associativity means
    1/2/3 is illegal

bnf grammar:

PROGRAMS
    program -> sc1; .. ;scn
        n >= 1
    
SUPERCOMBINATORS
    sc -> var var1 .. varn = expr
        n >= 0
    
EXPRESSIONS
    expr -> expr aexpr
        | expr1 binop expr2
        | let defns in expr
        | letrec defns in expr
        | case expr of alts
        | \ var1 .. varn . expr 
            n >= 1
        | aexpr 
    
    Atomic
    aexpr -> var | num | Pack{num,num} | ( expr )
    
DEFINITIONS
    defns -> defn1; .. defn
        n >= 1
    defn = var = expr
    
ALTERNATIVES
    alts -> alt1; .. ;altn
    alt -> <num> var1 .. varn -> expr 
        n >= 0
    
BINARY OPS
    binop -> arithop | relop | boolop
    arithop -> + | - | * | / 
    relop -> < | <= | >= | > | == | ~=
    boolop -> & | |
    
VARIABLES
    var -> alpha varch1..varchn 
        n >= 0
    alpha -> alphabetic man!
    varch -> alpha | digit | _
    
NUMBEROS
    num -> digit1..digitn
        n >= 1

Check the datatypes too.

Prelude:
will be used throughout the universe. 

I x    = x ;
K x y  = x ;
K1 x y = y ;

S f g x       = f x (g x) ;
compose f g x = f   (g x) ;
twice f       = compose f f

> preludeDefs :: CoreProgram
> preludeDefs 
>  = [ ("I", ["x"], EVar "x")
>    , ("K", ["x", "y"], EVar "x")
>    , ("K1", ["x", "y"], EVar "y")

>    , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x"))
>                                 (EAp (EVar "g") (EVar "x")))
>    , ("compose", ["f", "g", "x"], EAp (EVar "f")
>                                       (EAp (EVar "g") (EVar "x")))
>    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
>    ] 






> main :: IO ()
> main = 
>   putStrLn "type the core program filename." >>
>   getLine >>= readFile >>=
>   interpret >> main

> interpreters = concatMap ("\n\t" ++) 
>   [ "(0) - pretty print it"
>   , "(1) - TBD"]

> interpret f = do
>   putStrLn $ "which interpreter to use?" ++ interpreters
>   i <- getLine
>   case i of 
>       "0" -> (putStrLn . pprint . parse) f  --must to parse 
>       "p" -> (putStrLn . pprint) preludeDefs
>       _ -> putStrLn "try that again." >> interpret f

> tryParse :: FilePath -> IO ()
> tryParse fname = do
>   f <- readFile fname
>   mapM_ print (clex f 1)
>   putStr "\n\n\nRemaining input:\n"
>   let dbg = parseDebug f
>   print $ fst dbg
>   mapM_ print (snd dbg)
>   putStrLn "\npretty:"
>   (putStrLn . pprint . parse) f --must to parse 
