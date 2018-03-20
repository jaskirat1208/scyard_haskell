module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.Example
 
main :: IO ()
main = do
        putStr "Chief: Tom, a notorious criminal has escaped from the prison and has vanished into the city. You, in a team of five: Mark, Mike, Marcus, James, Crick have to go and find him and bring him back."
        putStr "Mark : Ok sir, but do we have any clues about him ?"
        putStr "Chief: Yes. You know that he is hidden inside the state somewhere in between the seven islands. So, we have blocked all the outgoing paths for the exit. Now the you five have to go around and find him. For this, you have been given tickets for the same. You have 15 taxi, 10 bus, 7 rail tickets. Mark has 7 taxi, 5 bus and 4 rail tickets only. Don't underestimate him, he's really clever. As more and more transactions occur, tickets are supplied to him. For example, if Mike spends one taxi to go from one place to another, then Mark would fetch an extra taxi ticket with him. Also, he can track your locations as well. So, be careful and get him as soon as possible. Get him before he reaches his hideout or your tickets run out. Use your tickets wisely as they cannot be swapped with another. The following is a map of the city: \n"
        print state
        putStr "NOTE: The state has seven islands, each of which has a central rail, which can be used to access other cities. They are internally connected with buses and taxis as well. The player who is playing Tom must keep a track of his position by himself since we are not going to reveal his position. "
        setup_game
        print ("Good game, well played")

----------------------------------------------------------------------------------------------------------------------------------------------------
--Player DESCRIPTION--------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------

state::Gr String (Int, Int, Int, Int)
state = mkGraph [(1,"a"),(2,"b"),(3,"c")] [(1,2,(1,0,1,0)), (1,3,(1,0,1,1))]

type Name  = String
type Taxi_t= Int
type Rail_t= Int
type Bus_t = Int
type Black_t = Int
type Detective = (Name,Taxi_t,Rail_t, Bus_t)

type Criminal = (Name, Taxi_t,Rail_t,Bus_t,Black_t)

mark::Detective 
mark = ("Mark", 15, 7, 10)

marcus::Detective
marcus = ("Marcus", 15, 7, 10)

mike::Detective
mike = ("Mike", 15, 7, 10)

james::Detective
james = ("James", 15, 7, 10)

crick::Detective
crick = ("Crick", 15, 7, 10)

tom::Criminal
tom = ("Tom", 7, 5, 4, 2)


setup_game = do
                print mark
                print marcus
                print mike
                print james
                print crick
make_next_move _ = 0 
----------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------
