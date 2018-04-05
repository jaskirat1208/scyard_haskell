module Display (display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Cube
 
display :: DisplayCallback
display = do
	let color3f r g b = color $ Color3 r g (b :: GLfloat)
	let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
	clear [ColorBuffer]
	-- drawVertex 0 0 0
	-- renderPrimitive Points $ do
	-- 	color3f 1 0 0
	scale 1.5 1.5 (1.5::GLfloat)

	--ISLANDS------------------------------------------------------
	drawIsland (-0.3) (-0.5) (0)
	drawIsland (-0.3) (0.5) (0)
	drawIsland (0.3) (0.5) (0)
	drawIsland (0.3) (-0.5) (0)
	drawIsland (-0.5) (-0.04) (0)
	drawIsland (0.5) (-0.04) (0)
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

  -- forM_ (points 7) $ \(x,y,z) ->
  --   preservingMatrix $ do
  --     color $ Color3 ((4::GLfloat)/5) ((3::GLfloat)/5) (0::GLfloat)
  --     translate $ Vector3 x y z
  --     cube 0.05
