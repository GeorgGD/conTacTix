{-
PRECONDITION: the resolution of your screen must be 1366x768 
-}

module Main(main) where
 
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.IO.Game hiding (Point)

import GameRules



{-
  template
  PRE:
  RETURNS:
  VARIANTS:
  SIDE-EFFECTS:
  EXAMPLES:
-}
  


--Constants 
width, height, offsetS :: Int
width = 1200
height = 700
offsetS = 0

{-
  window 
  window contains the parameters for the GUI
  RETURNS: the title, size and position of the GUI.
  EXAMPLES: window
            will return
            InWindow "Con-Tac-Tix" (1200,700) (0,0)
-}
window :: Display
window = InWindow "Con-Tac-Tix" (width, height) (offsetS, offsetS)

backgroundColor :: Color
backgroundColor = dark $ dark magenta

{-
  drawing board 
  RETURNS: a Picture element with positioning, color and object.  
-}

drawing :: Board -> Picture
drawing board = scale 0.9 0.9 $  pictures $ world (board) ++ [wallsWhite 373 (47), wallsWhite (-238) (47), wallsBlack 69 350, wallsBlack 69 (-255)] 


{-
  glossBoard
  creates the points in the con-tac-tix board
  RETURNS: a list of circles.  
-}
  
glossBoard :: [Picture]
glossBoard = glossBoard' 12 0 0

{-
  glossBoard' x a b 
  creates the points in the con-tac-tix board with x being the size (x*x) of the board
  PRE: a and b must have value 0 when the function is initially called
  RETURNS: a list of circles. 
  VARIANTS: value a increasing until it's equal to x.
-}
glossBoard' :: Int -> Int -> Int -> [Picture]
glossBoard' x a b | a == x =  []
                  | mod a x <= (x-1) = (glossRowBoard x a b) ++ (glossBoard' x (a+1) b)


{-
  glossRowBoard x a b 
  creates a list of circles  
  PRE: a and b must have value 0 when the function is initially called
  RETURNS: a list of circles.
  VARIANTS: value b increasing until it's equal to x-1.
-}
glossRowBoard :: Int -> Int -> Int -> [Picture]
glossRowBoard x a b | mod b x == (x-1) = [makeBallPoint a b]
                    | mod b x  < (x-1) = [makeBallPoint a b] ++ (glossRowBoard x a (b+1))

{-
  makeBallPoint a b 
  creates a solid circle at index points a and b.
  RETURNS: a solid circle.
-}
makeBallPoint :: Int -> Int -> Picture
makeBallPoint a b = ball (fromIntegral ((-228)+50*a) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor0

ballColor0, ballColor1, ballColor2 :: Color
ballColor0 = greyN 0.6
ballColor1 = black
ballColor2 = white

{-
  ball y x col
  creates a solid circle with position (y,x), as well as color col
-}
ball :: Float -> Float -> Color -> Picture
ball y x col = translate x y $ color col $ circleSolid 24

{-
 wallsWhite a b
 wallsBlack a b 
 draws rectangle on the board, of length a and width b
-}
wallsWhite :: Float -> Float -> Picture
wallsWhite a b = translate a b $ color white $ rectangleSolid 10 600
wallsBlack a b = translate a b $ color black $ rectangleSolid 600 10 

{-
  checkPress x y 
  checks to see if player pressed a point in the GUI within desired regiion. 
  RETURNS: the point on the board that was pressed.
  EXAMPLES: checkPress 205 (-15)
            will return
            (7,11)

-}

checkPress :: Float -> Float -> (Int,Int)
checkPress x y | (x > (-265) && y > (-226)) && (x < (206) && y < (296)) = checkPressPos x y 12 0 0 
               | otherwise = (20,20)
{-
  checkPressPos x y n a b 
  checks at what point the user places a pown.  
  PRE: a and b must have value 0 when the function is initially called.
  RETURNS: the coordinates of the point where the pown was placed.
  VARIANTS: value a increasing until it's equal to x.
  EXAMPLES: checkPressPos 200 (-210) 12 0 0
            will return
            (11,11)
-}
checkPressPos :: Float -> Float -> Int -> Float -> Float -> (Int,Int)
checkPressPos x y n a b | n == ((round a) :: Int) = (20,20)
                         | n == ((round b) :: Int) = checkPressPos x y n (a+1) 0  
                         | 20 >= sqrt((x -((-250)+40*b))^2+(y-((281)-44*a))^2) = ((round a) :: Int ,(round b) :: Int)
                         | otherwise = checkPressPos x y n a (b+1) 


{-
  mouseInputs event board 
  looks for mouse presses and paints a circle in the board either black or white.   
  PRE: a mouse with left and righ button
  RETURNS: if event is within board, a board with a placed piece. If the move is game winning, the board is reset.
  SIDE-EFFECT: draws a circle of either color black or white on the board 
  EXAMPLES: mouseInputs (EventKey (MouseButton LeftButton) Down _ (-420, 322)) [(Empty (0,0)),(Empty (0,0)),(Empty (0,0)),(Empty (0,0))]
            will return
            [(Black (0,0)),(Empty (0,0)),(Empty (0,0)),(Empty (0,0))]
-}
mouseInputs :: Event -> Board -> Board
mouseInputs (EventKey (MouseButton LeftButton) Down _ (x, y)) board | (winCon (makeMove (toPointB (checkPress x y)) board) (Black (0,0))) == True = makeBoard 12
                                                                   | (makeMove (toPointB (checkPress x y)) board) /= board = (makeMove (toPointB (checkPress x y)) board)
                                                                   
mouseInputs (EventKey (MouseButton RightButton) Down _ (x, y)) board | (winCon (makeMove (toPointW (checkPress x y)) board) (White (0,0))) == True = makeBoard 12
                                                                    | (makeMove (toPointW (checkPress x y)) board) /= board = (makeMove (toPointW (checkPress x y)) board)
mouseInputs _ board = board

{-
  world (x:xs)   
  takes a representation of the board and draws it.  
  RETURNS: a list with solid circles.
  VARIANTS: length of xs.
  EXAMPLES: world [(Black (0,0)),(Empty (0,0)),(Empty (0,0)),(Empty (0,0))]
            will return
            [Translate (-208.0) 322.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 12.0 24.0)),Translate (-208.0) 322.0 (Color (RGBA 0.6 0.6 0.6 1.0) (ThickCircle 12.0 24.0)),Translate (-208.0) 322.0 (Color (RGBA 0.6 0.6 0.6 1.0) (ThickCircle 12.0 24.0)),Translate (-208.0) 322.0 (Color (RGBA 0.6 0.6 0.6 1.0) (ThickCircle 12.0 24.0))]
-}
world :: Board -> [Picture]
world [] = []
world (b:bs) = (drawBall b):(world bs)

{-
  drawBall x   
  it creates a circle at a given point in the board.  
  RETURNS: a solid circle.
  EXAMPLES: drawBall (Black (0,2))
            will return
            Translate (-108.0) 322.0 (Color (RGBA 0.0 0.0 0.0 1.0) (ThickCircle 12.0 24.0))
-}
drawBall :: Point -> Picture 
drawBall (Empty (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor0
drawBall (White (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor2
drawBall (Black (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor1


-----------------------------------------
 

----------- Rendering
main :: IO ()
main = play window backgroundColor 1 (makeBoard 12) (drawing) (mouseInputs) (flip const)


