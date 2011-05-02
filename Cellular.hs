{-
    Basic cellular automata. Read comments for more info.

    To try it out:
      [in ghci] genOut (genLoop <nr. of runs> seed)
-}

-- The Seed type.
type Generation = String
-- The Rule type.
type Rules = [[String]]

-- The print-out
genOut :: Generation -> IO ()
genOut a = putStr (a ++ "\n")

-- Generator loop. Generates the cellular automata. Recursive function.
genLoop :: Int -> Generation -> Generation
genLoop 0 gen = "\n"  -- Base case.
genLoop x gen = (applyRules gen rules (length gen)) ++ "\n" ++ (genLoop (x - 1) (applyRules gen rules (length gen)))

-- The string to work on. 
--  NOTE: The chars on the rims (head and last) will remain constant throughout the generations, although they _will_ continue to influence the adjecent cell. -}
seed :: Generation
seed = ".................#.........................."

-- The rules for the cellular automata. These rules will, in the future, be imported from the arguments passed to the program (or supplied by the user via a text file).
-- Note: This thing is hairier than Chewbacca on Rogaine. It _MUST_ be flattened out.
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
        | (length gen) == lnth                                    = (head gen):[] ++ (applyRules gen rules (lnth - lnth)) -- Hack to make the first run apply the head and then go to the default.
        | (length gen) == 3                                       = [] ++ (tail gen)                                      -- Just output the chars.
        | lnth == 0                                               = [] ++ (returnRule (take 3 gen) rules) ++ (applyRules (drop 1 gen) rules (length gen)) -- Defaults.
        | otherwise                                               = [] ++ (returnRule (take 3 gen) rules) ++ (applyRules (drop 1 gen) rules (length gen)) -- Defaults.

-- returnRule matches the given pattern, ptrn, with the appropriate rule.
returnRule :: String -> Rules -> String
returnRule ptrn rls = head (head [tail x | x <- rls, ptrn == (head x)])
