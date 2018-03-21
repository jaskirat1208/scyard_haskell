module GeeksLand where

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


vertices_of_hill_demo = [(z,z) | z <- [85..112]]
bus_edges_of_hill_demo = (concat [[(84+i,84+2*i+1,(0,1,0)),(84+i,84+2*i+2,(0,1,0))]| i<-[2..10]]) ++ [(85,86,(0,1,0)), (85,87,(0,1,0)),(85,88,(0,1,0))]
taxi_edges_of_hill_demo = [(84+i,i+85,(1,0,0)) | i <- [2,3] ++ [5..9] ++ [11..21] ] ++ [(88,86,(1,0,0)),(94,89,(1,0,0)),(106,95,(1,0,0))]

city::Gr Int (Int, Int, Int)
city = mkGraph vertices_of_hill_demo (bus_edges_of_hill_demo ++ taxi_edges_of_hill_demo)


city_countryside_connectors = [(i,107+(i-95) `div` 2,(1,0,0) )| i<-[95..106]] ++ [(107 + (i `div` 2), j, (1,0,0) )| (i,j)<-(zip [0..] [12,13,26,27,40,41,54,55,68,69,82,83])]        --edges that will connect city and countryside: vertices: 107 to 112

flight_connectors = [(85,i,(0,0,1))| i <- [1,15,29,43,57,71] ] ++ [(i,j,(0,0,1))| (i,j) <- zip [1,15,29,43,57,71] [15,29,43,57,71,1] ]



--map = city U countryside  U city_countryside_connectors
geeksland::Gr Int (Int, Int, Int)
geeksland = mkGraph ((labNodes city) ++ (labNodes countryside)) ((labEdges city) ++ (labEdges countryside)++ city_countryside_connectors ++ flight_connectors)



