findPartition [] _ = []
findPartition l s 
    | s >= v = 1 : findPartition (tail l) (s - v)
    | otherwise = -1 : findPartition (tail l) s
    where v = head l

solvePartition l
    | even s = Just . reverse $ findPartition (reverse l) (s `div` 2)
    | otherwise = Nothing 
    where s = sum l