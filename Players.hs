module Players where 
import Data.Graph.Inductive
import GeeksLand 
import Data.List
type Name  = String
type Foot= Int
type Ropeway= Int
type Heli = Int
type Black_t = Int
type Position = Node
type Detective = (Name,Position,Foot,Ropeway, Heli)
type Criminal = (Name,Position,Foot,Ropeway,Heli,Black_t)
type PlayerState = ([Detective],Criminal)
mark::Detective 
mark = ("Mark", 2,15, 7, 10)

marcus::Detective
marcus = ("Marcus", 14, 15, 7, 10)

mike::Detective
mike = ("Mike", 39, 15, 7, 10)

james::Detective
james = ("James", 45, 15, 7, 10)

crick::Detective
crick = ("Crick", 56, 15, 7, 10)

tom::Criminal
tom = ("Tom", 87, 7, 5, 4, 2)

players::PlayerState
players = ([mark,marcus,mike,james,crick],tom)


--Deletes an element from its index
deleteAt idx xs = lft ++ rgt  where (lft, (_:rgt)) = splitAt idx xs


getDetective gs turn = ((fst gs) !! (turn-1))
getKiller gs _    = snd gs

getKillerPosition gs turn = x where (a,x,c,d,e,f) = getKiller gs turn
getDetectivePosition gs turn = x where (a,x,c,d,e) = getDetective gs turn


checkDetectiveTicket gs turn "foot" = c/=0 where  (a,b,c,d,e) = getDetective gs turn 
checkDetectiveTicket gs turn "ropeway" = d/=0 where (a,b,c,d,e) = getDetective gs turn
checkDetectiveTicket gs turn "heli" = e/=0 where (a,b,c,d,e) = getDetective gs turn
checkDetectiveTicket gs turn ticket = False

checkKillerTicket gs turn "foot" = c/=0 where  (a,b,c,d,e,f) = getKiller gs turn
checkKillerTicket gs turn "ropeway" = d/=0 where (a,b,c,d,e,f) = getKiller gs turn
checkKillerTicket gs turn "heli" = e/=0 where (a,b,c,d,e,f) = getKiller gs turn
checkKillerTicket gs turn "black" = f/=0 where (a,b,c,d,e,f) = getKiller gs turn 
checkKillerTicket gs turn ticket = False


node_check gs node turn | turn == 6 = find (==node) (neighbors geeksland (getKillerPosition gs turn)) /= Nothing
                        | otherwise = find (==node) (neighbors geeksland (getDetectivePosition gs turn)) /= Nothing


ticket_exis_check::PlayerState ->String->Int->Bool
ticket_exis_check gs ticket turn | turn == 6 = checkKillerTicket gs turn ticket 
                                 | otherwise = checkDetectiveTicket gs turn ticket

genLabelFromTicket "foot" = (1,0,0) 
genLabelFromTicket "ropeway" = (0,1,0) 
genLabelFromTicket "heli" = (0,0,1) 

--There is an edge corresponding to the ticket given

match_check gs node "black" 6 = True
match_check gs node ticket 6 = (find (==(node,getKillerPosition gs 6,genLabelFromTicket ticket)) (labEdges geeksland) /=Nothing) || (find (==(getKillerPosition gs 6,node,genLabelFromTicket ticket)) (labEdges geeksland) /=Nothing)
match_check gs node ticket turn = (find (==(node,getDetectivePosition gs turn,genLabelFromTicket ticket)) (labEdges geeksland) /=Nothing) || (find (==(getDetectivePosition gs turn,node,genLabelFromTicket ticket)) (labEdges geeksland) /=Nothing)

updateState gs x "black" 6 = ((fst gs),(a,x,c,d,e,f-1)) where (a,b,c,d,e,f) = getKiller gs 6   
updateState gs x "foot" 6  = ((fst gs),(a,x,c-1,d,e,f)) where (a,b,c,d,e,f) = getKiller gs 6   
updateState gs x "ropeway" 6  = ((fst gs),(a,x,c,d-1,e,f)) where (a,b,c,d,e,f) = getKiller gs 6   
updateState gs x "heli" 6  = ((fst gs),(a,x,c,d,e-1,f)) where (a,b,c,d,e,f) = getKiller gs 6   
updateState gs x "heli"     turn = ((take (turn-1) (fst gs)) ++ [(a,x,c,d,e-1)] ++ (drop turn (fst gs)),(l,m,n,o,p+1,q)) where  (a,b,c,d,e) = getDetective gs turn
                                                                                                                                (l,m,n,o,p,q) = getKiller gs turn
updateState gs x "foot"     turn = ((take (turn-1) (fst gs)) ++ [(a,x,c-1,d,e)] ++ (drop turn (fst gs)),(l,m,n+1,o,p,q)) where  (a,b,c,d,e) = getDetective gs turn
                                                                                                                                (l,m,n,o,p,q) = getKiller gs turn
updateState gs x "ropeway"  turn = ((take (turn-1) (fst gs)) ++ [(a,x,c,d-1,e)] ++ (drop turn (fst gs)),(l,m,n,o+1,p,q)) where  (a,b,c,d,e) = getDetective gs turn
                                                                                                                                (l,m,n,o,p,q) = getKiller gs turn


check_valid_move::PlayerState -> Node -> String -> Int -> (Bool,PlayerState)
check_valid_move gs x y turn | (node_check gs x turn ) && (ticket_exis_check gs y turn) && (match_check gs x y turn ) = (True,(updateState gs x y turn))
                             | otherwise = (False,gs)

-- CHECK_VALID MOVE SHALL perform sequential checks: node check, ticket and edge match check and ticket existence check,
-- move::PlayerState->Int->IO(PlayerState)
-- move players 6 = do 
--                     x_str <- getLine
--                     let x = (read x_str ::Int)
--                     let orig_killer = snd players
--                     let new_killer = (a,x,c,d,e,f) where (a,b,c,d,e,f) = orig_killer
--                     print new_killer
--                     return (fst players,new_killer)

-- move players y = do
--     let x = (read x_str ::Int)
--     if(not (fst (check_valid_move players x y_str y) ))
--         then do
--                 print "ILLEGAL MOVE. Please move again."
--                 move players y
--                 return players
--     else
--         do
--             putStrLn "Move Complete. Final State: "
--             print (snd (check_valid_move players x y_str y))
--                         return (snd (check_valid_move players x y_str y)) 