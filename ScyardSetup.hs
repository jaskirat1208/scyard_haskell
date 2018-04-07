module ScyardSetup where
----------------------------------------------------------------------------------------------------------------------------------------------------
--Player DESCRIPTION--------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
import Data.Graph.Inductive
import Data.Graph.Inductive.Example
import Players
import Data.List
state::Gr Int (Int, Int, Int, Int)
state = mkGraph (zip [1..103] [1..103]) [(1,2,(1,0,1,0)), (1,3,(1,0,1,1))]
--main_hill: Nodes from 79 to 103
--hillocks: Nodes from 1 to 78


--get_positions_list::PlayerState -> [Node]

detective_pose::Detective->Node
detective_pose det = x where (a,x,c,d,e)=det
get_positions_list gs = (killer_pose (snd gs)) ++ (map detective_pose (fst gs)) 

killer_pose::Criminal->[Node]
killer_pose kil = [l] where (a,l,c,d,e,f) = kil

tom_is_caught::PlayerState ->Bool
tom_is_caught gs | find (==(head (get_positions_list gs))) (tail (get_positions_list gs)) /= Nothing = True
                 | otherwise = False

game_init::PlayerState -> Int -> IO(Int)
game_init players x = if( tom_is_caught players == True)   then   
                            do 
                                print "MARK IS CAUGHT. GAME ENDS"
                                return 1
                        else
                            do 
                                new_gs <- move players (1+x)
                                flag <- game_init new_gs ((x+1) `mod` 6)
                                return flag
