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
  
----------------- Rendering------------------------

width, height, offsetS :: Int
width = 1200
height = 700
offsetS = 0

{-
window draws the GUI with "Con-Tac-Tix" being ther title of the window, width, height and offsetS are self explanatory. 
SIDE-EFFECT: creates a GUI
-}
window :: Display
window = InWindow "Con-Tac-Tix" (width, height) (offsetS, offsetS)

backgroundColor :: Color
backgroundColor = dark $ dark magenta

{-
SIDE-EFFECT: draws the board when the game is launched.
-}
drawing :: Board -> Picture
drawing board = scale 0.9 0.9 $  pictures $ world (board) ++ [wallsWhite 373 (47), wallsWhite (-238) (47), wallsBlack 69 350, wallsBlack 69 (-255)] 


{-
  glossBoard
  draws the con-tac-tix board
  RETURNS: a 
  SIDE-EFFECTS: draws solid circles (in GUI).
  EXAMPLES: lol
-}
  
glossBoard :: [Picture]
glossBoard = glossBoard' 12 0 0

{-
  glossBoard' x a b 
  draws the points in the con-tac-tix board with x being the size (x*x) of the board
  PRE: a and b must have value 0 when the function is initially called
  RETURNS: a list of objects. 
  VARIANTS: value a increasing until it's equal to x.
  SIDE-EFFECTS: draws solid circles (in GUI).
  EXAMPLES: lol
-}
glossBoard' :: Int -> Int -> Int -> [Picture]
glossBoard' 0 _ _ = []
glossBoard' x a b | a == x =  []
                  | mod a x <= (x-1) = (glossRowBoard x a b) ++ (glossBoard' x (a+1) b)


{-
  glossRowBoard x a b 
  creates a list of circles  
  PRE: a and b must have value 0 when the function is initially called
  RETURNS: a list of circles.
  VARIANTS: value b increasing until it's equal to x-1.
  SIDE-EFFECTS: draws solid circles (in GUI).
  EXAMPLES: lol
-}
glossRowBoard :: Int -> Int -> Int -> [Picture]
glossRowBoard x a b | mod b x == (x-1) = [makeBallPoint a b]
                    | mod b x  < (x-1) = [makeBallPoint a b] ++ (glossRowBoard x a (b+1))

{-
  makeBallPoint a b 
  creates a solid circle at index points a and b.
  RETURNS: a solid circle
  SIDE-EFFECTS: draws a single solid circle (in GUI).
  EXAMPLES: lol
-}
makeBallPoint :: Int -> Int -> Picture
makeBallPoint a b = ball (fromIntegral ((-228)+50*a) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor0

ballColor0, ballColor1, ballColor2 :: Color
ballColor0 = greyN 0.6
ballColor1 = black
ballColor2 = white

{-
  ball offset x col
  creates a solid circle with position offset and x, as well as color col
  SIDE-EFFECT: draws a solid circle
-}
ball :: Float -> Float -> Color -> Picture
ball offset x col = translate x offset $ color col $ circleSolid 24

{-
 wallsWhite a b
 wallsBlack a b 
 draws rectangle on the board, of length a and width b
 SIDE-EFFECT: draws a rectangle
-}
wallsWhite a b = translate a b $ color white $ rectangleSolid 10 600
wallsBlack a b = translate a b $ color black $ rectangleSolid 600 10 

---------------------------Gameplay------------------------------------
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

--------------------------------------------------------------------------------------------
                
glossBoard1 ::  Color -> [Picture]
glossBoard1 color = glossBoard1' 12 0 0 color 
                        


glossBoard1' :: Int -> Int -> Int -> Color -> [Picture]
glossBoard1' 0 _ _ color = []
glossBoard1' x a b  color | a == x =  []
                          | mod a x <= (x-1) = (glossRowBoard1 x a b color) ++ (glossBoard1' x (a+1) b color)


glossRowBoard1 :: Int -> Int -> Int -> Color -> [Picture]
glossRowBoard1 x a b color| mod b x == (x-1) = [makeBallPoint1 a b color]
                     | mod b x  < (x-1) = [makeBallPoint1 a b color] ++ (glossRowBoard1 x a (b+1) color)

makeBallPoint1 :: Int -> Int -> Color ->  Picture
makeBallPoint1 a b color = ball (fromIntegral ((-228)+50*a) ::Float) (fromIntegral ((-208)+50*b) :: Float) color


{-
  handleKeys x board ------------
  checks at what point the user places a pown.  
  PRE: a and b must have value 0 when the function is initially called.
  RETURNS: the coordinates of the point where the pown was placed.
  VARIANTS: value a increasing until it's equal to x.
  EXAMPLES: checkPressPos 200 (-210) 12 0 0
            will return
            (11,11)
-}
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) board | (winCon (makeMove (toPointB (checkPress x y)) board) (Black (0,0))) == True = makeBoard 12
                                                                   | (makeMove (toPointB (checkPress x y)) board) /= board = (makeMove (toPointB (checkPress x y)) board)
                                                                   
handleKeys (EventKey (MouseButton RightButton) Down _ (x, y)) board | (winCon (makeMove (toPointW (checkPress x y)) board) (White (0,0))) == True = makeBoard 12
                                                                    | (makeMove (toPointW (checkPress x y)) board) /= board = (makeMove (toPointW (checkPress x y)) board)
handleKeys _ board = board
iteration _ glossBoard = glossBoard
-------------------------------------------------------------------------------------------------

{-
  world (x:xs)   -------- 
  checks at what point the user places a pown.  
  PRE: a and b must have value 0 when the function is initially called.
  RETURNS: the coordinates of the point where the pown was placed.
  VARIANTS: value a increasing until it's equal to x.
  EXAMPLES: checkPressPos 200 (-210) 12 0 0
            will return
            (11,11)
-}
world :: Board -> [Picture]
world [] = []
world (b:bs) = (drawBall b):(world bs)

--world board = scale 0.82 0.82 $  rotate (45) $ pictures $ board ++ [wallsWhite 373 (54), wallsWhite (-238) (38), wallsBlack 62 350, wallsBlack 74 (-255)] 
 
drawBall :: Point -> Picture 
drawBall (Empty (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor0
drawBall (White (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor2
drawBall (Black (a,b)) = ball (fromIntegral ((-228)+50*(11-a)) ::Float) (fromIntegral ((-208)+50*b) :: Float) ballColor1


-----------------------------------------
 

----------- Rendering
main :: IO ()
--main = display window background (world z)
main = play window backgroundColor 1 s (drawing) (handleKeys) (iteration)


s = makeBoard 12
--s = [Empty (0,0),Empty (0,1),Empty (0,2),Black (0,3),Black (0,4),Empty (0,5),Empty (1,0),Empty (1,1),Empty (1,2),Empty (1,3),Black (1,4),Empty (1,5),Empty (2,0),Empty (2,1),Empty (2,2),Empty (2,3),Black (2,4),Empty (2,5),Empty (3,0),Empty (3,1),Black (3,2),Black (3,3),Empty (3,4),Empty (3,5),Empty (4,0),Empty (4,1),Black (4,2),Empty (4,3),Empty (4,4),Empty (4,5),Empty (5,0),Empty (5,1),Black (5,2),Empty (5,3),Empty (5,4),Empty (5,5)]
