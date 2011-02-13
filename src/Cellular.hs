{-
    Basic cellular automata. Read comments for more info.
-}

-- The Seed type.
type Generation = String
-- The Rule type.
type Rules = [[String]]

-- Adds two generations together for printing.
--concatGen :: Generation -> Generation -> Generation
--concatGen gen1 gen2 = gen1 ++ "\n" ++ gen2

genOut :: Generation -> IO ()
genOut a = putStr (a ++ "\n")

-- Generator loop.
genLoop :: Int -> Generation -> Generation
genLoop 0 gen = "\n"
genLoop x gen = (applyRules gen rules (length gen)) ++ "\n" ++ (genLoop (x - 1) (applyRules gen rules (length gen)))

{- The nested list to work on. 
   NOTE: The chars on the rims (head and tail) will remain constant throughout the generations, while they _will_ continue to influence the adjecent cell. -}
seed :: Generation
seed = ".................#.........................."

-- The rules for the cellular automata. These rules will, in the future, be imported from the arguments passed to the program (or supplied by the user via a text file).
rules :: Rules
rules = [ ["...", "."], 
          ["..#", "#"],
          [".#.", "#"],
          [".##", "."],
          ["#..", "#"],
          ["#.#", "#"],
          ["##.", "."],
          ["###", "."]
        ]

-- Applies rules to a Generation (which could be the seed) to produce a new nested list of strings ([Char]), i.e. a new "Generation".
applyRules :: Generation -> Rules -> Int -> Generation
applyRules gen rules lnth
        | (length gen) == lnth                                    = (head gen):[] ++ (applyRules gen rules (lnth - lnth))
        | (length gen) == 3                                       = [] ++ (tail gen)
        | lnth == 0                                               = [] ++ (returnRule (take 3 gen) rules) ++ (applyRules (drop 1 gen) rules (length gen))
        | otherwise                                               = [] ++ (returnRule (take 3 gen) rules) ++ (applyRules (drop 1 gen) rules (length gen))

returnRule :: String -> Rules -> String
returnRule ptrn rls = head (head [tail x | x <- rls, ptrn == (head x)])
