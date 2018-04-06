module Points where
 
import Graphics.Rendering.OpenGL
 
points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (0, 0, 1) | k <- [1..n'] ]
   where n' = fromIntegral n


mypoints = [(0,1,0),(0,0.5,0),(0.5,0.5,0.0)] 
--points shall keep the track of the positions of the players: This will be used to indicate the position of the