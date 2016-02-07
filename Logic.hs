-- https://wiki.haskell.org/Logic_programming_example

import Control.Monad (guard)

data Sex = Male | Female deriving (Show, Eq)

data PuzzleAnswer = PuzzleAnswer {
    parent1    :: Sex,
    parent2    :: Sex,
    child      :: Sex,
    child_desc :: Sex
}

instance Show PuzzleAnswer where
    show pa = "Parent1 is " ++ (show $ parent1 pa) ++ "\n" ++
              "Parent2 is " ++ (show $ parent2 pa) ++ "\n" ++
              "The child is" ++ (show $ child pa) ++ "\n" ++
              "The child said they were " ++ (show $ child_desc pa) ++ "\n"

childs_statement_is_valid :: Sex -> Sex -> Bool
childs_statement_is_valid Male Female = False
childs_statement_is_valid _ _         = True


parent1_statement_is_valid :: Sex -> Sex -> Bool
parent1_statement_is_valid Male Female = False
parent1_statement_is_valid _ _         = True


parent2_statement_is_valid :: Sex -> Sex -> Sex -> Bool
parent2_statement_is_valid Male Female Male = True
parent2_statement_is_valid Female _ Female  = True
parent2_statement_is_valid _ _ _            = True

solve_puzzle :: (Sex -> Sex -> Bool) -> [PuzzleAnswer]
solve_puzzle sexuality_pred = do
    parent1    <- [Male, Female]
    parent2    <- [Male, Female]
    child      <- [Male, Female]
    child_desc <- [Male, Female]
    guard $ sexuality_pred parent1 parent2
    guard $ childs_statement_is_valid child child_desc
    guard $ parent1_statement_is_valid parent1 child_desc
    guard $ parent2_statement_is_valid parent2 child child_desc
    return PuzzleAnswer {
          parent1    = parent1
        , parent2    = parent2
        , child      = child
        , child_desc = child_desc
        }

main = do
    putStrLn "---------- Heterosexual Couple ---------"
    mapM_ print (solve_puzzle (/=))
    putStrLn "---------- Gay Couple ---------"
    mapM_ print (solve_puzzle (\x y -> x == y && x == Male))
    putStrLn "---------- Lesbian Couple ---------"
    mapM_ print (solve_puzzle (\x y -> x == y && x == Female))
