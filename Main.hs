{-
-}

data Point = Empty
 (Int,Int) | White (Int,Int) | Black (Int,Int) deriving (Show)



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
sortBoard (x:xs) = [sortBoardByRow(x:xs)] ++ sortBoard(removeRow x (x:xs))
                              
comparePoints :: Point -> Point -> Bool
comparePoints (Empty (a1,_)) (Empty (a2,_)) | a1 == a2 = True
                                            | otherwise = False
                                            
sortBoardByRow :: Board -> Board
sortBoardByRow [] = []
sortBoardByRow (x:xs) | length xs == 0 = [x]
                      | comparePoints x (head xs) == True = x:(sortBoardByRow xs)
                      | comparePoints x (head xs) == False = [x] 

removeRow :: Point -> Board -> Board
removeRow _ [] = []
removeRow k (x:xs) | comparePoints k x == True = removeRow k xs
                   | comparePoints k x == False = (x:xs)
    

emptyPoint :: Point -> Bool
emptyPoint (Empty (a,b)) = True
emptyPoint _ = False
