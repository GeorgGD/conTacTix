{-
-}

data Point = Empty (Int,Int) | White (Int,Int) | Black (Int,Int) deriving (Show,Eq)


{- sameColor point point
  Checks if two points has the same color.
  PRE: True
  RETURNS: True if the given points has the same color, otherwise it will return False.
  EXAMPLES:  sameColor (White (2,4)) (White (1,3))   will return   True
-}


sameColor :: Point -> Point -> Bool
sameColor (White (_,_)) (White (_,_)) = True
sameColor (Empty (_,_)) (Empty (_,_)) = True
sameColor (Black (_,_)) (Black (_,_)) = True
sameColor _ _ = False

type Board = [Point]



{-
  makePoint a b
  Creates an unoccupied point with the given co-ordinates (a,b)
  RETURNS: The empty point with coordinates (a,b)
  EXAMPLES: makePoint 1 1 = Empty (1,1)
-}
makePoint :: Int -> Int -> Point
makePoint a b = Empty (a,b)


{-
  makeBoard x
  creates a Board of size x^2
  RETURNS: Board with empty points.
  PRE: x must be non-negative.
  EXAMPLES: makeBoard 3   will return    [Empty (0,0),Empty (0,1),Empty (0,2),Empty (1,0),Empty (1,1),Empty (1,2),Empty (2,0),Empty (2,1),Empty (2,2)]
            makeBoard 2   will return    [Empty (0,0),Empty (0,1),Empty (1,0),Empty (1,1)]
-}

makeBoard :: Int -> Board
makeBoard x = makeBoard' x 0 0


{-
  makeBoard' x a b
  creates a board that is represented by a square matrix with starting index (0,0).
  PRE: x must be non-negative.
  RETURNS: returns a list of positions in a board, represented by color and a pair. 
  VARIANT: difference of x and a. (to be revised) 
  EXAMPLES:
-}

makeBoard' :: Int -> Int -> Int -> Board
makeBoard' 0 _ _ = []
makeBoard' x a b | a == x =  []
                | mod a x <= (x-1) = (rowBoard x a b) ++ (makeBoard' x (a+1) b)

{-
  rowBoard x a b
  creates a row of points
  PRE: x, a and b must be positive.
  RETURNS: all the empty points that exist with value a in board x, starting with the point of values a and b.
  VARIANT: difference between b and x. 
  EXAMPLES:  rowBoard 5 1 2   will return   [Empty (1,2),Empty (1,3),Empty (1,4)]
             rowBoard 4 1 1   will return   [Empty (1,1),Empty (1,2),Empty (1,3)]
-}

rowBoard :: Int -> Int -> Int -> Board
rowBoard x a b | mod b x == (x-1) = [makePoint a b]
                | mod b x  < (x-1) = [makePoint a b] ++ (rowBoard x a (b+1))

{-
  emptyPoint point
  checks if a point is empty or not      
  PRE: True
  RETURNS: True if a point is empty, otherwise False.
  EXAMPLES:  emptyPoint (Empty (2,5))   will return   True
             emptyPoint (White (3,1))   will return   False
-}

emptyPoint :: Point -> Bool
emptyPoint (Empty (a,b)) = True
emptyPoint _ = False

{-
  pointPos (p (a,b)) 
  extracts the position, values a and b, from a point without its color.
  PRE: True
  RETURNS: a 2-tuple with the values a and b from the given point.
  EXAMPLES: pointPos (White (1,4))   will return   (1,4)
            pointPos (Empty (0,4))   will return   (0,4)
-}

pointPos :: Point -> (Int,Int)
pointPos (Empty (a,b)) = (a,b)
pointPos (Black (a,b)) = (a,b)
pointPos (White (a,b)) = (a,b)



{-
  makeMove (p (a,b)) board
  changes the color in the given board from Empty to the color of the given point in the point with same values a and b as the given point.
  PRE: True
  RETURNS: the same board, but with the empty point (with same values a and b as the given point) replaced with the given point. If the original point is not empty then the given board will be returned untouched.
  VARIANT: length board
  EXAMPLES: makeMove (White (1,1)) (makeBoard 2)   will return   [Empty (0,0),Empty (0,1),Empty (1,0),White (1,1)]
            makeMove (Black (1,1)) [Empty (0,0),Empty (0,1),Empty (1,0),White (1,1)]    will return   [Empty (0,0),Empty (0,1),Empty (1,0),White (1,1)]
-}

makeMove :: Point -> Board -> Board
makeMove _ [] = []
makeMove p@(White (a,b)) (x:xs) | x /= (Empty (a,b)) = [x] ++ makeMove p xs
                                | otherwise          = (p:xs)
makeMove p@(Black (a,b)) (x:xs) | x /= (Empty (a,b)) = [x] ++ makeMove p xs
                                | otherwise          = (p:xs)
makeMove (Empty (a,b)) board                         = board


{-
  connectedDots d1 d2
  checks if d1 and d2 are connected. TODO describe what points are connected
  PRE: d1 and d2 must be pairs that contain positive integers.
  RETURNS: True if the two given dots are connected, otherwise False.
  EXAMPLES: connectedDots (1,1) (1,2)   will return   True
            connectedDots (1,1) (2,2)   will reutrn   False
-}

connectedDots :: (Int,Int) -> (Int,Int) -> Bool
connectedDots (a1,b1) (a2,b2) | (a1 - a2) == 1 && (b1-b2) ==1 = False
                              | (a1 - a2) == (-1) && (b1-b2) == (-1) = False
                              | abs (a1-a2) <= 1 && abs (b1-b2) <= 1 = True
                              | otherwise = False

{-
  connectedPoints p1 p2
  checks if two points are connected (have connected Dots as well as the same color). TODO describe what points are connected
  PRE: True
  RETURNS: True if the two given points are connected, otherwise False.
  EXAMPLES: connectedPoints (White (1,1)) (White (1,2))   will return   True
            connectedPoints (White (1,1)) (Black (1,2))   will reutrn   False
-}
connectedPoints :: Point -> Point -> Bool
connectedPoints p1 p2 | (sameColor p1 p2) && connectedDots (pointPos p1) (pointPos p2) = True
                      | otherwise = False

{- 
  neighbours point board
  finds all the neighbours (connected points) to the given point in the given board.
  PRE: True
  RETURNS: a list with all the points that are neighbours (connected) to the given point.
  VARIANT: length board
  EXAMPLES: neighbours (White (1,1)) [Empty (0,0),Empty (0,1),White (1,0),Empty (1,1)]   will return   [White (1,0)]
            neighbours (Empty (1,1)) [Empty (0,0),Empty (0,1),Black (0,2),Empty (1,0),Empty (1,1),Black (1,2),Empty (2,0),Empty (2,1),Empty (2,2)]   will return   [Empty (0,1),Empty (0,2),Empty (1,0),Empty (1,1),Empty (1,2),Empty (2,0),Empty (2,1)]
-}

neighbours :: Point -> Board -> Board
neighbours p [] = []
neighbours p (x:xs) | (connectedPoints p x) == True = x : neighbours p xs
                    | otherwise = neighbours p xs

{-
  pointRemover board n
  removes a point inside a board
  RETURNS: a board with point n removed
  EXAMPLES: pointRemover 
-}
pointRemover :: Board -> Point -> Board
pointRemover [] _ = []
pointRemover (b:bs) n | n == b = bs
                 | otherwise =  b:(pointRemover bs n)
                 
{-
  pointRemover2 board (n:ns)
  removes all given points inside a board
  RETURNS: a board with all points (n:ns) removed
  EXAMPLES: pointRemover2 
-}
pointRemover2 :: Board -> Board -> Board
pointRemover2 [] _ = []
pointRemover2 board [] = board
pointRemover2 (b:bs) (n:ns) | n == b = pointRemover2 bs ns
                            | otherwise = pointRemover2 (pointRemover (b:bs) n) ns


{-
  crawler board p
  goes through a board and looks for all connected points in the board.  
  RETURNS: all the connected points in a board.
  EXAMPLES: crawler 
-}
crawler :: Board -> Point -> Board
crawler [] _ = []
crawler _ (Empty (_,_)) = []
crawler board p = crawlerAux board [p] []


{-
  crawlerAux board (p:ps) cP
  goes through a board and looks for all connected points in the board.  
  RETURNS: all the connected points in a board.
  EXAMPLES: crawlerAux 
-}
crawlerAux :: Board -> Board -> Board -> Board
crawlerAux board [] [] = []
crawlerAux board [(Empty (_,_))] [] = []
crawlerAux board [] cP = cP   
crawlerAux board (p:ps) cP = crawler b x ++ crawlerAux b ps [] ++ crawlerAux b xs l     
  where
    (x:xs) | (neighbours p board) == [] = [(Empty (0,0))]
           | otherwise = neighbours p board
    l | (x:xs) == [(Empty (0,0))] = cP
      | otherwise = cP ++ (x:xs)
    b = pointRemover2 board (x:xs)



{-
  blackSide board 
  creates a list of all points on the top edge of the board.  
  RETURNS: all points on the board that represent the top edge.
  EXAMPLES: blackSide 
-}
blackSide :: Board -> Board  
blackSide [] = []
blackSide ((Empty (a,b)):ps) | a == 0 = (Empty (a,b)):(blackSide ps)
blackSide ((Black (a,b)):ps) | a == 0 = (Black (a,b)):(blackSide ps)                             
blackSide ((White (a,b)):ps) | a == 0 = (White (a,b)):(blackSide ps)
blackSide (p:ps) = []


{-
  whiteSide board 
  creates a list of all points on the left edge of the board.  
  RETURNS: all points on the board that represent the left edge of the board.
  EXAMPLES: whiteSide 
-}
whiteSide :: Board -> Board
whiteSide [] = []
whiteSide ((Empty (a,b)):ps) | b == 0 = (Empty (a,b)):(whiteSide ps)
whiteSide ((Black (a,b)):ps) | b == 0 = (Black (a,b)):(whiteSide ps)
whiteSide ((White (a,b)):ps) | b == 0 = (White (a,b)):(whiteSide ps)
whiteSide (p:ps) =  whiteSide ps

{-
  blackEndSide board 
  creates a list of all points on the buttom edge of the board.  
  RETURNS: all points on the board that represent the buttom side of the board.
  EXAMPLES: blackEndSide 
-}
blackEndSide :: Board -> Board -> Board
blackEndSide [] _ = []
blackEndSide ((Empty (a,b)):ps) board | a == x = (Empty (a,b)):(blackEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
blackEndSide ((Black (a,b)):ps) board | a == x = (Black (a,b)):(blackEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
blackEndSide ((White (a,b)):ps) board | a == x = (White (a,b)):(blackEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
blackEndSide (p:ps) board = blackEndSide ps board


{-
  whiteEndSide board 
  creates a list of all points on the right edge of the board.  
  RETURNS: all points on the board that represent the left side of the board.
  EXAMPLES: whiteEndSide 
-}
whiteEndSide :: Board -> Board -> Board
whiteEndSide [] _ = []
whiteEndSide ((Empty (a,b)):ps) board | b == x = (Empty (a,b)):(whiteEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
whiteEndSide ((Black (a,b)):ps) board | b == x = (Black (a,b)):(whiteEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
whiteEndSide ((White (a,b)):ps) board | b == x = (White (a,b)):(whiteEndSide ps board)
  where
    x = (length (blackSide board)) - 1 
whiteEndSide (p:ps) board = whiteEndSide ps board

{-
  startCon board (x:xs)  
  looks to see if the input list of points (x:xs) has a non empty point on the board  
  RETURNS: True if the input list (x:xs) has a non empty point.
  EXAMPLES: startCon 
-}
startCon :: Board -> Board -> Bool
startCon board [] = False
startCon board (s:ss) | (crawler board s) == [] = startCon board ss
                      | otherwise = True

{-
  endCon board (x:xs) (y:ys) endPoints 
  looks to see if the list of points (x:xs) has a point that exist in list (y:ys)  
  RETURNS: True if a point in (x:xs) is found in (y:ys).
  EXAMPLES: startCon 
-}
endCon :: Board -> Board -> Board -> Board -> Bool
endCon board _ [] _ = False 
endCon board [] (x:xs) endPoints = endCon board endPoints xs endPoints
endCon board (e:es) (x:xs) endPoints | sameColor e x = True
                                     | otherwise = endCon board es (x:xs) endPoints

{-
  listOfConPoints board (x:xs)  
  shows all connected points from the first row of the board. 
  RETURNS: a list of list element who consist of connected points from the first row of the board.  
  EXAMPLES: listOfConPoints 
-}
listOfConPoints :: Board -> Board -> [Board]
listOfConPoints _ [] = []
listOfConPoints board (s:ss) | (crawler board s) == [] = listOfConPoints board ss
                             | otherwise = [(crawler board s)] ++ (listOfConPoints board ss)


{-
  winCon board color  
  looks to see if the player of given color has won the game. 
  RETURNS: true if the player has won, otherwise it returns false.  
  EXAMPLES: winCon 
-}
winCon :: Board -> Point -> Bool
winCon board color | sameColor color (Black (0,0)) = winCondition board (blackSide board) (blackEndSide board board)
                   | sameColor color (White (0,0)) = winCondition board (whiteSide board) (whiteEndSide board board)
                   | otherwise = False

{-
  winCondition board start end  
  check if a point in start list is connected to a point in the end list. 
  RETURNS: true if their is a connection between start and end, otherwise false.  
  EXAMPLES: winCondition 
-}
winCondition :: Board -> Board -> Board -> Bool 
winCondition board start end | (startCon board start) = endConFeeder (c:cs)  
                             | otherwise = False 
  where
    endConFeeder [] = False
    endConFeeder (x:xs) | (endCon board end x end) == False = endConFeeder xs
                        | (endCon board end x end) == True = True
    (c:cs) = listOfConPoints board start


