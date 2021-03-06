

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import GeeksLand
import Players
import ScyardSetup
import Data.List
data Quadrup a b c d = Quadrup a b c d deriving Show

width, height, offset, fps :: Int
width = 1000
height = 1000
offset = 100
fps = 60

myScaleX, myScaleY :: Float
myScaleX = 650
myScaleY = 450

p1, p2, p3 :: Point
p1 = (-200, 0)
p2 = (200, 0)
p3 = (300, 100)


type AuxState = (PlayerState,(Int,Int))
type Move = (String, Int)
type GameState = (AuxState, Move) 
-- type FinalGa

showKillerTime::[Int]
showKillerTime = [1,6,12,18,24,28]


myListPoints :: [(Point,Int)]
myListPoints = [(createPoint (list_map x),x) | x <- [1..112]]

wonderFunction m = [createLine (list_map x) (list_map y) | (x,y) <- m ]

myListRedEdges, myListGreenEdges, myListYellowEdges :: [Path]
myListRedEdges = wonderFunction getRedEdges
myListGreenEdges = wonderFunction getGreenEdges
myListYellowEdges = wonderFunction getYellowEdges


window :: Display
window = InWindow "Nice Window" (width, height) (offset, offset)

background :: Color
background  = white

drawing :: ((Float, Float),Int) -> Picture
drawing ((x, y),z) = pictures [translate x y (color (light yellow) (circleSolid 10)), translate (x-7) (y-4) (renderTextBox 0.08 (show z))  ] 

colorEdges :: Color -> [Path] -> [Picture]
colorEdges a b = map ((color a). line) b

renderTextBox :: Float -> String -> Picture
renderTextBox x str = scale x x (text str)


static_images :: [Picture]
static_images =   (colorEdges red myListRedEdges)++(colorEdges green myListGreenEdges)++(colorEdges (dark yellow) myListYellowEdges) ++ (map drawing myListPoints) ++ []

extractAllPositionLabels ps year 	| find (==year) showKillerTime /= Nothing = (map (getDetectivePosition ps) [1..5] ) ++ [(getKillerPosition ps 0)]
									| otherwise = (map (getDetectivePosition ps) [1..5] )


extractTuple ps year = zip ( map (createPoint.list_map) (extractAllPositionLabels ps year)) ["A","B","C","D","E","F"]

displayPositions :: PlayerState -> Int -> [Picture]
displayPositions ps year =  map getPlayerPicture (extractTuple ps year)

displayTicketType::String -> [Picture]
displayTicketType str = [translate 0 (-300) (renderTextBox 0.2 str)]
displayCurrentNumber::Int -> Int -> [Picture]
displayCurrentNumber y turn | (turn == 6) = [ translate (275) (-300) (renderTextBox 0.2 "x")]++[ translate (300) (-300) (renderTextBox 0.2 "x")]++[ translate (325) (-300) (renderTextBox 0.2 ("x"))] 
							| otherwise =   [ translate (275) (-300) (renderTextBox 0.2 (show (y `div` 100)))]++[ translate (300) (-300) (renderTextBox 0.2 (show ( (y `mod` 100) `div` 10 ) ))]++[ translate (325) (-300) (renderTextBox 0.2 (show (y `mod` 10)))]

render :: GameState -> Picture
render gs 	| (snd.snd.fst) gs == 31 = pictures [translate (-200) 0 (renderTextBox 0.5 "GAME OVER "), translate (-270) (-100) (renderTextBox 0.5 "Well Played, Tom") ] 
			| tom_is_caught ((fst.fst) gs)==False = pictures $ static_images  ++ (displayPositions ((fst.fst) gs) ((snd.snd.fst) gs)) ++ (displayPlayerData ((fst.fst) gs) ((fst.snd.fst) gs)) ++ (displayTicketType ((fst.snd) gs)) ++ (displayCurrentNumber ((snd.snd) gs) ((fst.snd.fst) gs))
			| otherwise =  pictures [translate (-200) 0 (renderTextBox 0.5 "GAME OVER "), translate (-350) (-100) (renderTextBox 0.5 "Well Played, detectives") ]

displayPlayerData :: PlayerState -> Int -> [Picture]
displayPlayerData ps 7 = [translate (-400) (-300) (renderTextBox 0.2 "GAME OVER.")]
displayPlayerData ps 6 = [translate (-400) (-300) (renderTextBox 0.2 (getKillerString (getKiller ps 6))) ]
displayPlayerData ps x = [translate (-400) (-300) (renderTextBox 0.2 (getDetectiveString (getDetective ps x))) ]


getDetectiveString:: Detective -> String
getDetectiveString (str, _, a, b, c) = str++" R:"++(show a)++" G:"++(show b)++" Y:"++(show c)

getKillerString:: Criminal -> String
getKillerString (str, _, a, b, c, d) = str++" R:"++(show a)++" G:"++(show b)++" Y:"++(show c)++" B:"++(show d)

gameState = ((players,(1,1)),("----",0))

update::Float->GameState->GameState
update  _ gs = gs

handleKeys::Event->GameState->GameState
handleKeys (EventKey (Char 's') Up _ _) (auxState, (a, b)) = (auxState, ("ropeway", b))
handleKeys (EventKey (Char 'a') Up _ _) (auxState, (a, b)) = (auxState, ("foot", b))
handleKeys (EventKey (Char 'd') Up _ _) (auxState, (a, b)) = (auxState, ("heli", b))
handleKeys (EventKey (Char 'f') Up _ _) (auxState, (a, b)) = (auxState, ("black", b))
--Numeric keys
handleKeys (EventKey (Char '1') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 1)`mod`1000))
handleKeys (EventKey (Char '2') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 2)`mod`1000))
handleKeys (EventKey (Char '3') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 3)`mod`1000))
handleKeys (EventKey (Char '4') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 4)`mod`1000))
handleKeys (EventKey (Char '5') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 5)`mod`1000))
handleKeys (EventKey (Char '6') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 6)`mod`1000))
handleKeys (EventKey (Char '7') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 7)`mod`1000))
handleKeys (EventKey (Char '8') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 8)`mod`1000))
handleKeys (EventKey (Char '9') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 9)`mod`1000))
handleKeys (EventKey (Char '0') Up _ _) ((playerState,(turn,year)), (a, b)) = ((playerState,(turn,year)), (a, (10*b + 0)`mod`1000))
handleKeys (EventKey (SpecialKey KeyEnter) Up _ _ ) ps = handleKeyReturn ps
handleKeys _  s = s
handleKeyReturn::GameState -> GameState
handleKeyReturn ( (playerState,(turn,year)) , (a,b) ) 	|  	(fst (check_valid_move playerState b a turn) == True ) && (tom_is_caught playerState == False) = ((snd (check_valid_move playerState b a turn) , (1 + (turn) `mod` 6,if turn == 6 then (year+1) else year)), ("----",0))  									
														|	tom_is_caught playerState == True = ((playerState,(7,year)),("TOM LOSE",999))
														|  	otherwise = ( (playerState,(turn,year)) , (a,b) )

main :: IO ()
main = do
  	play window background fps gameState render handleKeys update 
  -- display window background $ render gameState 
-- play window background fps initialState render handleKeys update

createPoint::Quadrup Int Float Float Float -> (Float,Float)
createPoint (Quadrup a b c d) = ((myScaleX*b), (myScaleY*c))

createLine::Quadrup Int Float Float Float -> Quadrup Int Float Float Float -> Path
createLine (Quadrup _ a b _) (Quadrup _ c d _) = [(myScaleX*a, myScaleY*b), (myScaleX*c, myScaleY*d)]

playerPicture::Point->String->Picture
playerPicture (x, y) str = pictures [color red (translate x y (circleSolid 10)), (translate (x-3) (y-4) (renderTextBox 0.08 str))]

getPlayerPicture::(Point,String)->Picture
getPlayerPicture ((x, y), str) = playerPicture (x, y) str



list_map::Int -> Quadrup Int Float Float Float
list_map 1  = Quadrup 1 (-0.3) 0.5 0.0 
list_map 2  = Quadrup 2 (-0.35000002) 0.45 0.0
list_map 3  = Quadrup 3 (-0.35000002) 0.55 0.0
list_map 4  = Quadrup 4 (-0.25) 0.55 0.0
list_map 5  = Quadrup 5 (-0.25) 0.45 0.0
list_map 6  = Quadrup 6 (-0.35000002) 0.39 0.0
list_map 7  = Quadrup 7 (-0.4) 0.45 0.0
list_map 8  = Quadrup 8 (-0.4) 0.55 0.0
list_map 9  = Quadrup 9 (-0.35000002) 0.61 0.0
list_map 10 = Quadrup 10 (-0.25) 0.61 0.0
list_map 11 = Quadrup 11 (-0.20000002) 0.55 0.0
list_map 12 = Quadrup 12 (-0.20000002) 0.45 0.0
list_map 13 = Quadrup 13 (-0.25) 0.39 0.0
list_map 14 = Quadrup 14 0 0.6 0
list_map 15 = Quadrup 15 0.3 0.5 0.0
list_map 16 = Quadrup 16 0.25 0.55 0.0
list_map 17 = Quadrup 17 0.35000002 0.55 0.0
list_map 18 = Quadrup 18 0.35000002 0.45 0.0
list_map 19 = Quadrup 19 0.25 0.45 0.0
list_map 20 = Quadrup 20 0.20000002 0.55 0.0
list_map 21 = Quadrup 21 0.25 0.61 0.0
list_map 22 = Quadrup 22 0.35000002 0.61 0.0
list_map 23 = Quadrup 23 0.4 0.55 0.0
list_map 24 = Quadrup 24 0.4 0.45 0.0
list_map 25 = Quadrup 25 0.35000002 0.39 0.0
list_map 26 = Quadrup 26 0.25 0.39 0.0
list_map 27 = Quadrup 27 0.20000002 0.45 0.0
list_map 28 = Quadrup 28 (0.45) (0.25) 0
list_map 29 = Quadrup 29 0.5 (-0.04) 0.0
list_map 30 = Quadrup 30 0.5 0.01 0
list_map 32 = Quadrup 32 0.5 (-0.09) 0
list_map 33 = Quadrup 33 0.45 (-0.04) 0.0
list_map 31 = Quadrup 31 0.55 (-0.04) 0.0
list_map 34 = Quadrup 34 0.45 0.070 0.0
list_map 35 = Quadrup 35 0.55 0.07 0.0
list_map 36 = Quadrup 36 0.6 0.01 0.0
list_map 37 = Quadrup 37 0.6 (-0.09) 0.0
list_map 38 = Quadrup 38 0.55 (-0.15) 0.0
list_map 39 = Quadrup 39 0.45 (-0.15) 0.0
list_map 41 = Quadrup 41 0.4 0.01 0.0
list_map 40 = Quadrup 40 0.4 (-0.09) 0.0
list_map 42 = Quadrup 42 (0.45) (-0.25) 0
list_map 43 = Quadrup 43 0.3 (-0.5) 0.0
list_map 44 = Quadrup 44 0.35000002 (-0.45) 0.0
list_map 45 = Quadrup 45 0.35000002 (-0.55) 0.0
list_map 46 = Quadrup 46 0.25 (-0.55) 0.0
list_map 47 = Quadrup 47 0.25 (-0.45) 0.0
list_map 48 = Quadrup 48 0.35000002 (-0.39) 0.0
list_map 49 = Quadrup 49 0.4 (-0.45) 0.0
list_map 50 = Quadrup 50 0.4 (-0.55) 0.0
list_map 51 = Quadrup 51 0.35000002 (-0.61) 0.0
list_map 52 = Quadrup 52 0.25 (-0.61) 0.0
list_map 53 = Quadrup 53 0.20000002 (-0.55) 0.0
list_map 54 = Quadrup 54 0.20000002 (-0.45) 0.0
list_map 55 = Quadrup 55 0.25 (-0.39) 0.0
list_map 56 = Quadrup 56 0 (-0.56) 0
list_map 57 = Quadrup 57 (-0.3) (-0.5) 0.0
list_map 58 = Quadrup 58 (-0.25) (-0.55) 0.0
list_map 59 = Quadrup 59 (-0.35) (-0.55) 0.0
list_map 60 = Quadrup 60 (-0.35) (-0.45) 0.0
list_map 61 = Quadrup 61 (-0.25) (-0.45) 0.0
list_map 62 = Quadrup 62 (-0.20) (-0.55) 0.0
list_map 63 = Quadrup 63 (-0.25) (-0.60) 0.0
list_map 64 = Quadrup 64 (-0.35) (-0.60) 0.0
list_map 65 = Quadrup 65 (-0.40) (-0.55) 0.0
list_map 66 = Quadrup 66 (-0.40) (-0.45) 0.0
list_map 67 = Quadrup 67 (-0.35) (-0.40) 0.0
list_map 68 = Quadrup 68 (-0.25) (-0.40) 0.0
list_map 69 = Quadrup 69 (-0.20) (-0.45) 0.0
list_map 70 = Quadrup 70 (-0.45) (-0.25) 0
list_map 71 = Quadrup 71 (-0.5) (-0.04) 0.0
list_map 72 = Quadrup 72 (-0.5) (-0.09) 0.0
list_map 73 = Quadrup 73 (-0.55) (-0.04) 0.0
list_map 74 = Quadrup 74 (-0.5) (0.01) 0.0
list_map 75 = Quadrup 75 (-0.45) (-0.04) 0.0
list_map 76 = Quadrup 76 (-0.45) (-0.15) 0
list_map 77 = Quadrup 77 (-0.55) (-0.15) 0
list_map 78 = Quadrup 78 (-0.60) (-0.09) 0
list_map 79 = Quadrup 79 (-0.60) (0.01) 0
list_map 80 = Quadrup 80 (-0.55) (0.07) 0
list_map 81 = Quadrup 81 (-0.45) (0.07) 0
list_map 82 = Quadrup 82 (-0.40) (0.01) 0
list_map 83 = Quadrup 83 (-0.4) (-0.09) 0
list_map 84 = Quadrup 84 (-0.45) (0.25) 0
list_map 85 = Quadrup 85 0 0 0
list_map 86 = Quadrup 86 0 0.09 0 
list_map 87 = Quadrup 87 (0.080) (-0.06) 0
list_map 88 = Quadrup 88 (-0.080) (-0.06) 0
list_map 89 = Quadrup 89 (-0.080) (0.15) 0
list_map 90 = Quadrup 90 (0.080) (0.15) 0
list_map 91 = Quadrup 91 (0.19) 0 0
list_map 92 = Quadrup 92 (0.080) (-0.18) 0
list_map 93 = Quadrup 93 (-0.080) (-0.18) 0
list_map 94 = Quadrup 94 (-0.19) 0 0
list_map 95 = Quadrup 95 (-0.16) (0.21) 0
list_map 96 = Quadrup 96 (-0.04) (0.24) 0
list_map 97 = Quadrup 97 (0.04) (0.24) 0
list_map 98 = Quadrup 98 (0.16) (0.21) 0
list_map 99 = Quadrup 99 (0.24) (0.06) 0
list_map 100 = Quadrup 100 (0.24) (-0.07) 0
list_map 101 = Quadrup 101 (0.16) (-0.21) 0
list_map 102 = Quadrup 102 (0.04) (-0.24) 0
list_map 103 = Quadrup 103 (-0.04) (-0.24) 0
list_map 104 = Quadrup 104 (-0.16) (-0.21) 0
list_map 105 = Quadrup 105 (-0.24) (-0.07) 0
list_map 106 = Quadrup 106 (-0.24) (0.06) 0
list_map 112 = Quadrup 112 (-0.32) (0) 0
list_map 109 = Quadrup 109 (0.32) (0) 0
list_map 107 = Quadrup 107 (-0.1) (0.35) 0
list_map 108 = Quadrup 108 (0.1) (0.35) 0
list_map 111 = Quadrup 111 (-0.1) (-0.35) 0
list_map 110 = Quadrup 110 (0.1) (-0.35) 0
