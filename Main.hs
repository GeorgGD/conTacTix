{-
-}

data Point = Empty
 (Int,Int) | White (Int,Int) | Black (Int,Int) deriving (Show,Eq)


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
  PRE: x is non-negative
  EXAMPLES: makeBoard 3   will return    [Empty (0,0),Empty (0,1),Empty (0,2),Empty (1,0),Empty (1,1),Empty (1,2),Empty (2,0),Empty (2,1),Empty (2,2)]

-}

makeBoard :: Int -> Board
makeBoard x = makeBoard' x 0 0


{-
  makeBoard' x a b

  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

makeBoard' :: Int -> Int -> Int -> Board
makeBoard' 0 _ _ = []
makeBoard' x a b | a == x =  []
                | mod a x <= (x-1) = (colmBoard x a b) ++ (makeBoard' x (a+1) b)

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

colmBoard :: Int -> Int -> Int -> Board
colmBoard x a b | mod b x == (x-1) = [makePoint a b]
                | mod b x  < (x-1) = [makePoint a b] ++ (colmBoard x a (b+1))

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

sortBoard :: Board -> [Board]
sortBoard [] = []
sortBoard (x:xs) = [sortBoardByRow(x:xs)] ++ sortBoard(removeRow x (x:xs))

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

comparePoints :: Point -> Point -> Bool
comparePoints (Empty (a1,_)) (Empty (a2,_)) | a1 == a2 = True
                                            | otherwise = False
{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

sortBoardByRow :: Board -> Board
sortBoardByRow [] = []
sortBoardByRow (x:xs) | length xs == 0 = [x]
                      | comparePoints x (head xs) == True = x:(sortBoardByRow xs)
                      | comparePoints x (head xs) == False = [x] 

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

removeRow :: Point -> Board -> Board
removeRow _ [] = []
removeRow k (x:xs) | comparePoints k x == True = removeRow k xs
                   | comparePoints k x == False = (x:xs)

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

emptyPoint :: Point -> Bool
emptyPoint (Empty (a,b)) = True
emptyPoint _ = False

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

pointPos :: Point -> (Int,Int)
pointPos (Empty (a,b)) = (a,b)
pointPos (Black (a,b)) = (a,b)
pointPos (White (a,b)) = (a,b)



{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

makeMove :: Point -> Board -> Board
makeMove _ [] = []
makeMove p@(White (a,b)) (x:xs) | x /= (Empty (a,b)) = [x] ++ makeMove p xs
                                | otherwise          = (p:xs)
makeMove p@(Black (a,b)) (x:xs) | x /= (Empty (a,b)) = [x] ++ makeMove p xs
                                | otherwise          = (p:xs)
makeMove (Empty (a,b)) board                         = board


{-
  connectedDots t1 t2
  checks if p1 and p2 are connected (tuple-pairs)
  RETURNS: Bool

-}

connectedDots :: (Int,Int) -> (Int,Int) -> Bool
connectedDots (a1,b1) (a2,b2) | (a1 - a2) == 1 && (b1-b2) ==1 = False
                              | (a1 - a2) == (-1) && (b1-b2) == (-1) = False
                              | abs (a1-a2) <= 1 && abs (b1-b2) <= 1 = True
                              | otherwise = False

{-
  connectedPoints p1 p2
  checks if two points are connected TODO describe what points are connected

-}
connectedPoints :: Point -> Point -> Bool
connectedPoints p1 p2 | (sameColor p1 p2) && connectedDots (pointPos p1) (pointPos p2) = True
                      | otherwise = False

{-
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:

-}

neighbours :: Point -> Board -> Board
neighbours p [] = []
neighbours p (x:xs) | (connectedPoints p x) == True = x : neighbours p xs
                    | otherwise = neighbours p xs

