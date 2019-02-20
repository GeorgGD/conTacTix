{-


-}

data Point = Empty (Int,Int) | White (Int,Int) | Black (Int,Int) deriving (Show)



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
  RETURNS: Empty


-}

makeBoard :: Int -> Board
makeBoard x = makeBoard' x 0 0

makeBoard' :: Int -> Int -> Int -> Board
makeBoard' 0 _ _ = []
makeBoard' x a b | a == x =  []
                | mod a x <= (x-1) = (colmBoard x a b) ++ (makeBoard' x (a+1) b)

colmBoard :: Int -> Int -> Int -> Board
colmBoard x a b | mod b x == (x-1) = [makePoint a b]
                | mod b x  < (x-1) = [makePoint a b] ++ (colmBoard x a (b+1))

sortBoard :: Board -> [Board]
sortBoard [] = []
sortBoard (x:y:xs) | comparePoints x y == True = [[x]] ++ sortBoard (y:xs)
                   | otherwise = sortBoard (y:xs)
                              

sortBoard2 :: Board -> [Board]
sortBoard2 [] = []
sortBoard2 (x:y:xs) | comparePoints x y == True = [x:(sortBoard2 (y:xs))]
                    | otherwise = [[x]] ++ (sortBoard2 (y:xs))

comparePoints :: Point -> Point -> Bool
comparePoints (Empty (a1,_)) (Empty (a2,_)) | a1 == a2 = True
                                   | otherwise = False


--makeMove :: Point -> Board
--makeMove (White (a,b)) = 
--makeMove (Black (a,b)) =

--makeMoveAux :: Point -> Board -> Board
--makeMoveAux (White (a,b)) board | 
--makeMoveAux (Black (a,b)) board |



{-
  emptyPoint p
  checks if p is empty
  RETURNS: True if p is empty, otherwise False
  EXAMPLES: emptyPoint (Empty (1,2)) == True
            emptyPoint (White (3,3)) == False
-}

emptyPoint :: Point -> Bool
emptyPoint (Empty (a,b)) = True
emptyPoint _ = False