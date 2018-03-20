module Players where 
import Data.Graph.Inductive


type Name  = String
type Taxi_t= Int
type Rail_t= Int
type Bus_t = Int
type Black_t = Int
type Position = Node
type Detective = (Name,Position,Taxi_t,Rail_t, Bus_t)
type Criminal = (Name,Position,Taxi_t,Rail_t,Bus_t,Black_t)

mark::Detective 
mark = ("Mark", 2,15, 7, 10)

marcus::Detective
marcus = ("Marcus", 1, 15, 7, 10)

mike::Detective
mike = ("Mike", 3, 15, 7, 10)

james::Detective
james = ("James", 4, 15, 7, 10)

crick::Detective
crick = ("Crick", 5, 15, 7, 10)

tom::Criminal
tom = ("Tom", 6, 7, 5, 4, 2)
