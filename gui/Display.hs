module Display (display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Cube



data Quadrup a b c d = Quadrup a b c d deriving Show


createVector x y z 	| x>0 && y >0.1 = Vector3 (x*8000-200) ((y*8000)-(200::GLfloat)) z 
					| x<0 && y>0.1 = Vector3 (x*8000+200) ((y*8000)-(200::GLfloat)) z
					| x<0 && y<0.1 = Vector3 (x*8000+200) ((y*8000)+(200::GLfloat)) z
					| x>0 && y<0.1 = Vector3 (x*8000-200) ((y*8000)+(200::GLfloat)) z
					| otherwise = Vector3 (x*8000-200) ((y*8000)-(200::GLfloat)) z
dummyfunc (Quadrup a x y z) = preservingMatrix $ do
      -- color $ Color3 x y z
      translate $ createVector x y z
      renderString Roman (show a)
      translate $ Vector3 0 0 (0::GLfloat)

-- getparams n = ( list_map n 

list_map::Int -> Quadrup Int Float Float Float
list_map 85 = Quadrup 85 0 0 (1::GLfloat) 
list_map 86 = Quadrup 86 0 0.09 (1::GLfloat) 
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
list_map 30 = Quadrup 30 0.5 0.00 0
list_map 31 = Quadrup 31 0.5 (-0.09) 0
list_map 32 = Quadrup 32 0.45 (-0.04) 0.0
list_map 33 = Quadrup 33 0.55 (-0.04) 0.0
list_map 34 = Quadrup 34 0.45 0.070 0.0
list_map 35 = Quadrup 35 0.55 0.07 0.0
list_map 36 = Quadrup 36 0.6 0.01 0.0
list_map 37 = Quadrup 37 0.6 (-0.09) 0.0
list_map 38 = Quadrup 38 0.55 (-0.15) 0.0
list_map 39 = Quadrup 34 0.45 (-0.15) 0.0
list_map 40 = Quadrup 40 0.4 0.01 0.0
list_map 41 = Quadrup 41 0.4 (-0.09) 0.0
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
list_map 56 = Quadrup 56 0 (-0.6) 0
display :: DisplayCallback
display = do 
	let color3f r g b = color $ Color3 r g (b :: GLfloat)
	let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
	clear [ColorBuffer]
	-- drawVertex 0 0 0
	-- renderPrimitive Points $ do
	-- 	color3f 1 0 0
	scale 0.0002 0.0002 (0.0001::GLfloat)
  	-- stringWidth 50
  	forM_ ([1..55]) (\x -> dummyfunc (list_map x))
  	-- dummyfunc (list_map 85) 
 --  	dummyfunc 86 0 0.09 (1::GLfloat) 
 --  	dummyfunc 1 (-0.3) 0.5 0.0 
	-- dummyfunc 1 (-0.3) 0.5 0.0
	-- dummyfunc 2 (-0.35000002) 0.45 0.0
	-- dummyfunc 3 (-0.35000002) 0.55 0.0
	-- dummyfunc 4 (-0.25) 0.55 0.0
	-- dummyfunc 5 (-0.25) 0.45 0.0
	-- dummyfunc 6 (-0.35000002) 0.39 0.0
	-- dummyfunc 7 (-0.4) 0.45 0.0
	-- dummyfunc 8 (-0.4) 0.55 0.0
	-- dummyfunc 9 (-0.35000002) 0.61 0.0
	-- dummyfunc 10 (-0.25) 0.61 0.0
	-- dummyfunc 11 (-0.20000002) 0.55 0.0
	-- dummyfunc 12 (-0.20000002) 0.45 0.0
	-- dummyfunc 13 (-0.25) 0.39 0.0
  	-- translate $ Vector3 (2::GLfloat) (2::GLfloat)  0
  	-- renderString Roman "1"
  	scale 7500 7500 (15000::GLfloat)
	
	--ISLANDS------------------------------------------------------
	drawIsland (-0.3) (-0.5) (0)
	drawIsland (-0.3) (0.5) (0)
	drawIsland (0.3) (0.5) (0)
	drawIsland (0.3) (-0.5) (0)
	drawIsland2 (-0.5) (-0.04) (0)
	drawIsland2 (0.5) (-0.04) (0)
	----CITY---------------------------------------------------------
	-----------------------------------------------------------------
	
	drawVertex 0 0 0
	drawVertex 0 0.075 0
	drawVertex (-0.080) (-0.06) 0
	drawVertex (0.080) (-0.06) 0
	drawVertex (-0.19) 0 0
	drawVertex (-0.080) (0.15) 0
	drawVertex (0.080) (0.15) 0
	drawVertex (0.19) 0 0
	drawVertex (-0.080) (-0.18) 0
	drawVertex (0.080) (-0.18) 0
	drawVertex (-0.16) (-0.21) 0
	drawVertex (-0.24) (-0.07) 0
	drawVertex (-0.24) (0.06) 0
	drawVertex (-0.16) (0.21) 0
	drawVertex (-0.04) (0.24) 0
	drawVertex (-0.04) (-0.24) 0
	drawVertex (0.04) (0.24) 0
	drawVertex (0.04) (-0.24) 0
	drawVertex (0.16) (-0.21) 0
	drawVertex (0.24) (-0.07) 0
	drawVertex (0.24) (0.06) 0
	drawVertex (0.16) (0.21) 0
	drawVertex (0.04) (0.24) 0
	drawVertex (0.04) (-0.24) 0
		


	--ALL RED EDGES SHOWN BELOW--------------------------------------
	drawRedEdges 0 0.075 0 (-0.080) (-0.06) 0
	drawRedEdges (-0.080) (-0.06) 0 (0.080) (-0.06) 0
	drawRedEdges (0.080) (-0.06) 0 0 0.075 0
	drawRedEdges (-0.19) 0 0 (-0.080) (0.15) 0
	drawRedEdges (-0.080) (0.15) 0 (0.080) (0.15) 0
	drawRedEdges (0.080) (0.15) 0 (0.19) 0 0
	drawRedEdges (0.19) 0 0 (0.080) (-0.18) 0
	drawRedEdges (-0.080) (-0.18) 0 (0.080) (-0.18) 0
	drawRedEdges (-0.080) (-0.18) 0 (-0.19) 0 0
	drawRedEdges (-0.16) (-0.21) 0 (-0.24) (-0.07) 0
	drawRedEdges (-0.24) (-0.07) 0 (-0.24) (0.06) 0
	drawRedEdges (-0.24) (0.06) 0 (-0.16) (0.21) 0
	drawRedEdges (-0.16) (0.21) 0 (-0.04) (0.24) 0
	drawRedEdges (-0.04) (0.24) 0 (0.04) (0.24) 0
	drawRedEdges (0.04) (0.24) 0 (0.16) (0.21) 0
	drawRedEdges (0.16) (0.21) 0 (0.24) (0.06) 0
	drawRedEdges (0.24) (0.06) 0 (0.24) (-0.07) 0
	drawRedEdges (0.24) (-0.07) 0 (0.16) (-0.21) 0
	drawRedEdges (0.16) (-0.21) 0 (0.04) (-0.24) 0
	drawRedEdges (0.04) (-0.24) 0 (-0.04) (-0.24) 0
	drawRedEdges (-0.04) (-0.24) 0 (-0.16) (-0.21) 0
	-----------------------------------------------------------------
	-----------------------------------------------------------------
	-- drawVertex (-0.055) (-0.035) 0
	-- drawVertex (0.055) (-0.035) 0
	-- drawVertex (-0.055) 0.115 0
	-- drawVertex (0.055) (0.115) 0
	-- drawVertex (-0.12) 0.015 0
	-- renderPrimitive Lines $ do
	-- 	color3f 0 1 0
	drawGreenEdges 0 0 0 0 0.075 0
	drawGreenEdges 0 0 0 (-0.080) (-0.06) 0
	drawGreenEdges 0 0 0 (0.080) (-0.06) 0
	drawGreenEdges (-0.080) (-0.06) 0 (-0.19) 0 0
	drawGreenEdges (-0.080) (-0.06) 0 (-0.080) (-0.18) 0
	drawGreenEdges 0 0.075 0 (0.080) (0.15) 0
	drawGreenEdges 0 0.075 0 (-0.080) (0.15) 0
	drawGreenEdges (0.080) (-0.06) 0 (0.19) 0 0
	drawGreenEdges (0.080) (-0.06) 0 (0.080) (-0.18) 0
	drawGreenEdges (0.19) 0 0 (0.24) (-0.07) 0
	drawGreenEdges (0.19) 0 0 (0.24) (0.06) 0
	drawGreenEdges (0.080) (0.15) 0 (0.04) (0.24) 0
	drawGreenEdges (0.080) (0.15) 0 (0.16) (0.21) 0
	drawGreenEdges (-0.080) (0.15) 0 (-0.04) (0.24) 0
	drawGreenEdges (-0.080) (0.15) 0 (-0.16) (0.21) 0
	drawGreenEdges (-0.19) 0 0 (-0.24) (-0.07) 0
	drawGreenEdges (-0.19) 0 0 (-0.24) (0.06) 0
	drawGreenEdges (-0.080) (-0.18) 0 (-0.04) (-0.24) 0
	drawGreenEdges (-0.080) (-0.18) 0 (-0.16) (-0.21) 0
	drawGreenEdges (0.080) (-0.18) 0 (0.04) (-0.24) 0
	drawGreenEdges (0.080) (-0.18) 0 (0.16) (-0.21) 0
	
	 	
	-- drawTaxiEdges 
	
	-- 	vertex3f 0 0.015 0
	-- 	vertex3f 0 0.075 0
	-- 	vertex3f 0 0.015 0
	-- 	vertex3f (-0.055) (-0.035) 0
	-- 	vertex3f 0 0.015 0
	-- 	vertex3f (0.055) (-0.035) 0
	-- CONNECTORS -------------------------------------------------------------
	drawVertex (-0.32) (0) 0
	drawVertex (0.32) (0) 0
	drawVertex (-0.1) (0.35) 0
	drawVertex (0.1) (0.35) 0
	drawVertex (-0.1) (-0.35) 0
	drawVertex (0.1) (-0.35) 0
	drawVertex (0) (0.6) 0
	drawVertex (0) (-0.6) 0
	drawVertex (-0.45) (0.25) 0
	drawVertex (0.45) (0.25) 0
	drawVertex (-0.45) (-0.3) 0
	drawVertex (0.45) (-0.3) 0


	drawRedEdges (-0.32) (0) 0 (-0.24) (-0.07) 0
	drawRedEdges (-0.32) (0) 0 (-0.24) (0.06) 0
	drawRedEdges (0.32) (0) 0 (0.24) (-0.07) 0
	drawRedEdges (0.32) (0) 0 (0.24) (0.06) 0
	drawRedEdges (-0.32) (0) 0 (-0.4) (-0.09) 0
	drawRedEdges (-0.32) (0) 0 (-0.4) (0.01) 0
	drawRedEdges (0.32) (0) 0 (0.4) (-0.09) 0
	drawRedEdges (0.32) (0) 0 (0.4) (0.01) 0
	drawRedEdges (-0.1) (0.35) 0 (-0.16) (0.21) 0
	drawRedEdges (-0.1) (0.35) 0 (-0.04) (0.24) 0
	drawRedEdges (0.1) (0.35) 0 (0.16) (0.21) 0
	drawRedEdges (0.1) (0.35) 0 (0.04) (0.24) 0
	drawRedEdges (0.1) (-0.35) 0 (0.16) (-0.21) 0
	drawRedEdges (0.1) (-0.35) 0 (0.04) (-0.24) 0
	drawRedEdges (-0.1) (-0.35) 0 (-0.16) (-0.21) 0
	drawRedEdges (-0.1) (-0.35) 0 (-0.04) (-0.24) 0
	drawRedEdges (0.1) (-0.35) 0 (0.25) (-0.39) (0)
	drawRedEdges (0.1) (-0.35) 0 (0.20) (-0.45) (0)
	drawRedEdges (-0.1) (-0.35) 0 (-0.25) (-0.39) (0)
	drawRedEdges (-0.1) (-0.35) 0 (-0.20) (-0.45) (0)
	drawRedEdges (-0.1) (0.35) 0 (-0.2) (0.45) (0)
	drawRedEdges (-0.1) (0.35) 0 (-0.25) (0.39) (0)
	drawRedEdges (0.1) (0.35) 0 (0.2) (0.45) (0)
	drawRedEdges (0.1) (0.35) 0 (0.25) (0.39) (0)
	drawRedEdges (-0.45) (0.25) 0 (-0.45) (0.07) 0
	drawRedEdges (-0.45) (0.25) 0 (-0.55) (0.07) 0
	drawRedEdges (0.45) (0.25) 0 (0.45) (0.07) 0
	drawRedEdges (0.45) (0.25) 0 (0.55) (0.07) 0
	drawRedEdges (-0.45) (0.25) 0 (-0.35) (0.39) 0
	drawRedEdges (-0.45) (0.25) 0 (-0.40) (0.45) 0
	drawRedEdges (0.45) (0.25) 0 (0.35) (0.39) 0
	drawRedEdges (0.45) (0.25) 0 (0.40) (0.45) 0
	drawRedEdges (-0.45) (-0.3) 0 (-0.40) (-0.45) 0
	drawRedEdges (-0.45) (-0.3) 0 (-0.35) (-0.39) 0
	drawRedEdges (0.45) (-0.3) 0 (0.40) (-0.45) 0
	drawRedEdges (0.45) (-0.3) 0 (0.35) (-0.39) 0
	drawRedEdges (-0.45) (-0.3) 0 (-0.45) (-0.15) 0
	drawRedEdges (-0.45) (-0.3) 0 (-0.55) (-0.15) 0
	drawRedEdges (0.45) (-0.3) 0 (0.45) (-0.15) 0
	drawRedEdges (0.45) (-0.3) 0 (0.55) (-0.15) 0
	drawRedEdges (0) (0.6) 0 (-0.2) (0.55) 0
	drawRedEdges (0) (0.6) 0 (-0.25) (0.61) 0
	drawRedEdges (0) (0.6) 0 (0.2) (0.55) 0
	drawRedEdges (0) (0.6) 0 (0.25) (0.61) 0
	drawRedEdges (0) (-0.6) 0 (-0.2) (-0.55) 0
	drawRedEdges (0) (-0.6) 0 (-0.25) (-0.61) 0
	drawRedEdges (0) (-0.6) 0 (0.2) (-0.55) 0
	drawRedEdges (0) (-0.6) 0 (0.25) (-0.61) 0

	---------------------------------------------------------------------------
	flush
	-- scale 0.0001 0.0001 (0.0001::GLfloat)
	-- color3f 0 0 1
  	-- renderString Roman "1"
  	-- scale 15000 15000 (15000::GLfloat)

  -- forM_ (points 7) $ \(x,y,z) ->
  --   preservingMatrix $ do
  --     color $ Color3 ((4::GLfloat)/5) ((3::GLfloat)/5) (0::GLfloat)
  --     translate $ Vector3 x y z
  --     cube 0.05
