{-
  BEHÖVER VI DENNA?

  sortBoard board
  
  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:
-}

sortBoard :: Board -> [Board]
sortBoard [] = []
sortBoard (x:xs) = [sortBoardByRow(x:xs)] ++ sortBoard(removeRow x (x:xs))

{-
  comparePoints (Empty (a1,b1)) (Empty (a2,b2))
  checks if two empty points have the same value for a.
  PRE: True
  RETURNS: True if the given points have the same value for a, if not then False.
  EXAMPLES:  comparePoints (Empty (1,2)) (Empty (1,4))   will return   True
-}

comparePoints :: Point -> Point -> Bool
comparePoints (Empty (a1,_)) (Empty (a2,_)) | a1 == a2 = True
                                            | otherwise = False
{-
        ANVÄNDER VI DENNA?

  sortBoardByRow board
  
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
        ANVÄNDER VI DENNA?

  PRE: True
  RETURNS:
  VARIANT:
  EXAMPLES:
-}

removeRow :: Point -> Board -> Board
removeRow _ [] = []
removeRow k (x:xs) | comparePoints k x == True = removeRow k xs
                   | comparePoints k x == False = (x:xs)
