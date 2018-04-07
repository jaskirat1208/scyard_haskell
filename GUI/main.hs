import Graphics.UI.GLUT
import Bindings
import Control.Concurrent 

rendergraphics = do
	displayCallback $= display
  	reshapeCallback $= Just reshape
  	keyboardMouseCallback $= Just keyboardMouse

dummy 2 = print "over"
dummy n = do
	x <- getLine
	forkIO $ rendergraphics
	dummy (n-1)


main :: IO ()
main = do
  	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Hello World"
  	dummy 5
  	mainLoop


display2 = do
	clear [ColorBuffer]
