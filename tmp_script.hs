module Main where

import GeeksLand
import Data.Graph.Inductive        
import Data.Graph.Inductive.Example

-- create::[(a,b)] ->
create ls = [(show a)++ " -- " ++ (show b) ++";\n" | (a,b) <- ls ]

main = do
	putStrLn "graph mygraph {"
	putStr (concat (create (edges geeksland)))
	putStr "}"