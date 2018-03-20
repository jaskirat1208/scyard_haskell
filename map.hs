import Data.Graph.Inductive

--base_hillock::Int ->Gr Int (Int, Int, Int, Int)

vertices_of_base_hillock i = (zip [i..(i+12)] [1..])
