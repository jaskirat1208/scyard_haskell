module Cube where
 
import Graphics.UI.GLUT
 
drawVertex :: GLfloat ->GLfloat ->GLfloat -> IO ()
drawVertex x y z = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    renderPrimitive Quads $ do
        color3f 1 0 0
        vertex3f (x+0.007) y z
        vertex3f x (y-0.007) z
        vertex3f (x-0.007) y z
        vertex3f x (y+0.007) z
        
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
        -- renderString Roman "1"
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

drawGreenEdges :: GLfloat ->GLfloat ->GLfloat ->GLfloat ->GLfloat ->GLfloat -> IO ()
drawGreenEdges x y z a b c = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    renderPrimitive Lines $ do
        color3f 0 1 0
        vertex3f x y z
        vertex3f a b c

drawRedEdges :: GLfloat ->GLfloat ->GLfloat ->GLfloat ->GLfloat ->GLfloat -> IO ()
drawRedEdges x y z a b c = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
    let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    renderPrimitive Lines $ do
        color3f 1 0 0
        vertex3f x y z
        vertex3f a b c
drawIsland:: GLfloat ->GLfloat ->GLfloat -> IO ()
drawIsland x y z = do
    printToConsole x y z
    printToConsole (x+0.05) (y+0.05) z
    printToConsole (x+0.05) (y-0.05) z
    printToConsole (x-0.05) (y+0.05) z
    printToConsole (x-0.05) (y-0.05) z
    printToConsole (x-0.1) (y-0.05) z
    printToConsole (x-0.1) (y+0.05) z
    printToConsole (x+0.1) (y-0.05) z
    printToConsole (x+0.1) (y+0.05) z
    printToConsole (x-0.05) (y-0.11) z
    printToConsole (x-0.05) (y+0.11) z
    printToConsole (x+0.05) (y-0.11) z
    printToConsole (x+0.05) (y+0.11) z

    
    drawGreenEdges x y z (x+0.05) (y+0.05) z
    drawGreenEdges x y z (x-0.05) (y+0.05) z
    drawGreenEdges x y z (x-0.05) (y-0.05) z
    drawGreenEdges x y z (x+0.05) (y-0.05) z
    drawGreenEdges (x+0.05) (y+0.05) z (x+0.1) (y+0.05) z
    drawGreenEdges (x+0.05) (y+0.05) z (x+0.05) (y+0.11) z
    drawGreenEdges (x-0.05) (y+0.05) z (x-0.1) (y+0.05) z
    drawGreenEdges (x-0.05) (y+0.05) z (x-0.05) (y+0.11) z
    drawGreenEdges (x+0.05) (y-0.05) z (x+0.1) (y-0.05) z
    drawGreenEdges (x+0.05) (y-0.05) z (x+0.05) (y-0.11) z
    drawGreenEdges (x-0.05) (y-0.05) z (x-0.1) (y-0.05) z
    drawGreenEdges (x-0.05) (y-0.05) z (x-0.05) (y-0.11) z
    drawRedEdges (x+0.05) (y+0.05) z (x+0.05) (y-0.05) z
    drawRedEdges (x+0.05) (y-0.05) z (x-0.05) (y-0.05) z
    drawRedEdges (x-0.05) (y-0.05) z (x-0.05) (y+0.05) z
    drawRedEdges (x-0.05) (y+0.05) z (x+0.05) (y+0.05) z
    drawRedEdges (x-0.1) (y-0.05) z (x-0.1) (y+0.05) z
    drawRedEdges (x-0.1) (y+0.05) z (x-0.05) (y+0.11) z
    drawRedEdges (x-0.05) (y+0.11) z (x+0.05) (y+0.11) z
    drawRedEdges (x+0.05) (y+0.11) z (x+0.1) (y+0.05) z
    drawRedEdges (x+0.1) (y+0.05) z (x+0.1) (y-0.05) z
    drawRedEdges (x+0.1) (y-0.05) z (x+0.05) (y-0.11) z
    drawRedEdges (x+0.05) (y-0.11) z (x-0.05) (y-0.11) z
    drawRedEdges (x-0.05) (y-0.11) z (x-0.1) (y-0.05) z
    drawVertex x y z
    drawVertex (x+0.05) (y+0.05) z
    drawVertex (x+0.05) (y-0.05) z
    drawVertex (x-0.05) (y+0.05) z
    drawVertex (x-0.05) (y-0.05) z
    drawVertex (x-0.1) (y-0.05) z
    drawVertex (x-0.1) (y+0.05) z
    drawVertex (x+0.1) (y-0.05) z
    drawVertex (x+0.1) (y+0.05) z
    drawVertex (x-0.05) (y-0.11) z
    drawVertex (x-0.05) (y+0.11) z
    drawVertex (x+0.05) (y-0.11) z
    drawVertex (x+0.05) (y+0.11) z

drawIsland2:: GLfloat ->GLfloat ->GLfloat -> IO ()
drawIsland2 x y z = do
    printToConsole x y z
    printToConsole (x) (y+0.05) z
    printToConsole (x) (y-0.05) z
    printToConsole (x-0.05) (y) z
    printToConsole (x+0.05) (y) z
    printToConsole (x-0.1) (y-0.05) z
    printToConsole (x-0.1) (y+0.05) z
    printToConsole (x+0.1) (y-0.05) z
    printToConsole (x+0.1) (y+0.05) z
    printToConsole (x-0.05) (y-0.11) z
    printToConsole (x-0.05) (y+0.11) z
    printToConsole (x+0.05) (y-0.11) z
    printToConsole (x+0.05) (y+0.11) z

    drawVertex x y z
    drawVertex (x) (y+0.05) z
    drawVertex (x) (y-0.05) z
    drawVertex (x+0.05) (y) z
    drawVertex (x-0.05) (y) z
    drawVertex (x-0.1) (y-0.05) z
    drawVertex (x-0.1) (y+0.05) z
    drawVertex (x+0.1) (y-0.05) z
    drawVertex (x+0.1) (y+0.05) z
    drawVertex (x-0.05) (y-0.11) z
    drawVertex (x-0.05) (y+0.11) z
    drawVertex (x+0.05) (y-0.11) z
    drawVertex (x+0.05) (y+0.11) z
    drawGreenEdges x y z (x) (y+0.05) z
    drawGreenEdges x y z (x-0.05) (y) z
    drawGreenEdges x y z (x) (y-0.05) z
    drawGreenEdges x y z (x+0.05) (y) z
    -- drawGreenEdges (x) (y+0.05) z (x-0.1) (y+0.05) z
    drawGreenEdges (x) (y+0.05) z (x+0.05) (y+0.11) z
    drawGreenEdges (x) (y+0.05) z (x-0.05) (y+0.11) z
    drawGreenEdges (x) (y-0.05) z (x+0.05) (y-0.11) z
    drawGreenEdges (x) (y-0.05) z (x-0.05) (y-0.11) z
    drawGreenEdges (x-0.05) (y) z (x-0.1) (y+0.05) z
    drawGreenEdges (x-0.05) (y) z (x-0.1) (y-0.05) z
    drawGreenEdges (x+0.05) (y) z (x+0.1) (y+0.05) z
    drawGreenEdges (x+0.05) (y) z (x+0.1) (y-0.05) z
    -- drawGreenEdges (x+0.05) (y) z (x+0.05) (y-0.11) z
    -- drawGreenEdges (x-0.05) (y-0.05) z (x-0.1) (y-0.05) z
    -- drawGreenEdges (x-0.05) (y-0.05) z (x-0.05) (y-0.11) z
    drawRedEdges (x+0.05) (y) z (x) (y-0.05) z
    drawRedEdges (x+0.05) (y) z (x) (y+0.05) z
    drawRedEdges (x-0.05) (y) z (x) (y+0.05) z
    drawRedEdges (x-0.05) (y) z (x) (y-0.05) z
    drawRedEdges (x-0.1) (y-0.05) z (x-0.1) (y+0.05) z
    drawRedEdges (x-0.1) (y+0.05) z (x-0.05) (y+0.11) z
    drawRedEdges (x-0.05) (y+0.11) z (x+0.05) (y+0.11) z
    drawRedEdges (x+0.05) (y+0.11) z (x+0.1) (y+0.05) z
    drawRedEdges (x+0.1) (y+0.05) z (x+0.1) (y-0.05) z
    drawRedEdges (x+0.1) (y-0.05) z (x+0.05) (y-0.11) z
    drawRedEdges (x+0.05) (y-0.11) z (x-0.05) (y-0.11) z
    drawRedEdges (x-0.05) (y-0.11) z (x-0.1) (y-0.05) z

printToConsole x y z = do
    putStrLn ((show x)++" " ++ (show y)++" " ++ (show z)) 