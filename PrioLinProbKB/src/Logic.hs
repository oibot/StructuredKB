module Logic where

data Formula = Atom String
             | Formula `Disjunction` Formula
             | Formula `Conjunction` Formula
             | Formula `Implication` Formula
             | Formula `Equality` Formula
             | Negation Formula
             deriving Eq

instance Show Formula where
    show (Atom s) = s
    show (f1 `Disjunction` f2) = show f1 ++ " + " ++ show f2
    show (f1 `Conjunction` f2) = show f1 ++ "*" ++ show f2
    show (f1 `Implication` f2) = show f1 ++ " --> " ++ show f2
    show (f1 `Equality` f2) = show f1 ++ " <--> " ++ show f2
    show (Negation f) = "-" ++ show f
