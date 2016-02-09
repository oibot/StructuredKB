module Logic (
      Formula(..)
    , atoms
    , valuations
    , satisfies
    ) where

data Formula = Atom String
             | Formula `Disjunction` Formula
             | Formula `Conjunction` Formula
             | Formula `Implication` Formula
             | Formula `Equality` Formula
             | Negation Formula
             deriving Eq

instance Show Formula where
    show (Atom s) = s
    show (f1 `Disjunction` f2) = "(" ++ show f1 ++ " + " ++ show f2 ++ ")"
    show (f1 `Conjunction` f2) = "(" ++ show f1 ++  " * "  ++ show f2 ++ ")"
    show (f1 `Implication` f2) = "(" ++ show f1 ++ " --> " ++ show f2 ++ ")"
    show (f1 `Equality` f2) = "(" ++ show f1 ++ " <--> " ++ show f2 ++ ")"
    show (Negation f) = "(" ++ "-" ++ show f ++ ")"


atoms :: Formula -> [String]
atoms (Atom a)            = [a]
atoms (Negation f)        = atoms f
atoms (Disjunction f1 f2) = atoms f1 ++ atoms f2
atoms (Conjunction f1 f2) = atoms f1 ++ atoms f2
atoms (Implication f1 f2) = atoms f1 ++ atoms f2
atoms (Equality f1 f2)    = atoms f1 ++ atoms f2

type Valuation = [(String,Bool)]

valuations :: Formula -> [Valuation]
valuations form = let as = atoms form
                   in mapM (\a -> [(a,True), (a,False)]) as

class Satisfiable a where
    satisfies :: a -> Valuation -> Bool

instance Satisfiable Formula where
    satisfies (Conjunction f1 f2) v = satisfies f1 v && satisfies f2 v
    satisfies (Disjunction f1 f2) v = satisfies f1 v || satisfies f2 v
    satisfies (Implication f1 f2) v = not (satisfies f1 v) || satisfies f2 v
    satisfies (Equality f1 f2) v = satisfies f1 v == satisfies f2 v
    satisfies (Negation f) v = not (satisfies f v)
    satisfies (Atom a) v = case lookup a v of
                             Just v' -> v'
                             _       -> undefined

