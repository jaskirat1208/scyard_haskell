module ScyardSetup where
----------------------------------------------------------------------------------------------------------------------------------------------------
--Player DESCRIPTION--------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Players
state::Gr Int (Int, Int, Int, Int)
state = mkGraph (zip [1..103] [1..103]) [(1,2,(1,0,1,0)), (1,3,(1,0,1,1))]
--main_hill: Nodes from 79 to 103
--hillocks: Nodes from 1 to 78



setup_game = do
                print mark
                print marcus
                print mike
                print james
                print crick

tom_is_caught = True
make_next_move y | tom_is_caught == True = do
                                                putStrLn "Game Ends"
                 | y `mod` 6 == 0 = do 
                                                putStrLn "Mark's Turn"     
                 | y `mod` 6 == 1 = do          
                                                putStrLn "Marcus's Turn"
                 | y `mod` 6 == 2 = do 
                                                putStrLn "Mike's Turn"
                 | y `mod` 6 == 3 = do
                                                putStrLn "James' Turn"
                 | y `mod` 6 == 4 = do 
                                                putStrLn "Crick's Turn"
                 | y `mod` 6 == 5 = do
                                                putStrLn "Tom's Turn"
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
