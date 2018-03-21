import Data.Graph.Inductive        
import Data.Graph.Inductive.Example
base_hillock::Int ->Gr Int (Int, Int, Int)
base_hillock i = mkGraph (vertices_of_base_hillock i) (edges_of_base_hillock i)
vertices_of_base_hillock i = (zip [i..(i+12)] [i..])
--There is a slight change: Black tickets have been removed: Now black tickets are only used to navigate hiding ticket from the detectives. No special black ticket route is there
bus_edges i = [(i,i+2,(0,1,0)),(i,i+3,(0,1,0)),(i,i+1,(0,1,0)),(i,i+4,(0,1,0)),(i+7,i+2,(0,1,0)),(i+9,i+3,(0,1,0)),(i+5,i+1,(0,1,0)),(i+11,i+4,(0,1,0)),(i+8,i+2,(0,1,0)),(i+10,i+3,(0,1,0)),(i+6,i+1,(0,1,0)),(i+12,i+4,(0,1,0))] 
taxi_edges i = [(i,i+1,(1,0,0)) | i <- [2..4]++[6..12] ] ++ [(5,2,(1,0,0)),(13,6,(1,0,0))]
edges_of_base_hillock i = (bus_edges i) ++ (taxi_edges i)



conn_vertices = [(z,z) | z <- [14,28,42,56,70,84] ]
conn_edges_countryside = concat [ [(z,z-4,(1,0,0)),(z,z-3,(1,0,0)),(z,(z+6) `mod` 84,(1,0,0)), (z,(z+7) `mod` 84,(1,0,0)) ] |z <- [14,28,42,56,70,84]]
conn_edges = [] ++ conn_edges_countryside
vertices_of_hillocks = labNodes (base_hillock 1) ++ labNodes (base_hillock 15) ++ labNodes (base_hillock 29) ++ labNodes (base_hillock 43) ++ labNodes (base_hillock 57) ++ labNodes (base_hillock 71) ++ conn_vertices 
edges_of_hillocks = labEdges (base_hillock 1) ++ labEdges (base_hillock 15) ++ labEdges (base_hillock 29) ++ labEdges (base_hillock 43) ++ labEdges (base_hillock 57) ++ labEdges (base_hillock 71) ++ conn_edges



countryside::Gr Int (Int, Int, Int)
countryside = mkGraph (vertices_of_hillocks) (edges_of_hillocks)


