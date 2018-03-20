import Data.Graph.Inductive
import Data.Graph
base_hillock::Int ->Gr Int (Int, Int, Int)
base_hillock i = mkGraph (vertices_of_base_hillock i) (edges_of_base_hillock i)
vertices_of_base_hillock i = (zip [i..(i+12)] [i..])
--There is a slight change: Black tickets have been removed: Now black tickets are only used to navigate hiding ticket from the detectives. No special black ticket route is there
bus_edges i = [(i,i+2,(0,1,0)),(i,i+3,(0,1,0)),(i,i+1,(0,1,0)),(i,i+4,(0,1,0)),(i+7,i+2,(0,1,0)),(i+9,i+3,(0,1,0)),(i+5,i+1,(0,1,0)),(i+11,i+4,(0,1,0)),(i+8,i+2,(0,1,0)),(i+10,i+3,(0,1,0)),(i+6,i+1,(0,1,0)),(i+12,i+4,(0,1,0))] 
taxi_edges i = [(i,i+1,(1,0,0)) | i <- [2..4]++[6..12] ] ++ [(5,2,(1,0,0)),(13,6,(1,0,0))]
edges_of_base_hillock i = (bus_edges i) ++ (taxi_edges i)



conn_vertices = []
conn_edges = []
-- unify_graphs::Gr Int (Int, Int, Int) -> Gr Int (Int, Int, Int) -> Gr Int (Int, Int, Int)
-- unify_graphs::Graph gr => gr a b -> gr a b -> gr a b
-- unify_graphs gr x1 y1 (gr x2 y2) = mkGraph (x1++x2) (y1++y2)
