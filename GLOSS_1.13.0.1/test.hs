ballCoord = ballCoord' 12 0 0

ballCoord' k a b | b == k = []
                 | mod b k <= (k-1) = (ballCoordRow k a b) ++ (ballCoord' k a (b+1))

ballCoordRow k a b | a == (k-1) = [makeCoord a b]
                   | mod a k < (k-1) = [makeCoord a b] ++ (ballCoordRow k (a+1) b)

makeCoord a b = (((-318)+35*a+35*b),(10+35*a-35*b)) 

checkPressPos :: Float -> Float -> Int -> Float -> Float -> (Int,Int)
checkPressPos x y n a b | n == ((round a) :: Int) = (20,20)
                         | n == ((round b) :: Int) = checkPressPos x y n (a+1) 0  
                         | 20 >= sqrt((x -((-250)+40*b))^2+(y-((281)-44*a))^2) = ((round a) :: Int ,(round b) :: Int)
                         | otherwise = checkPressPos x y n a (b+1) 
