findJump [] _ = []
findJump v d l
    | l - h > d = 
    | otherwise = 
    where h = head v

solveJump v d = h : findJump (tail v) d h
    where h = head v