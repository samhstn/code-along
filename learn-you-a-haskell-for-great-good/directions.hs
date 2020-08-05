data Direction = North | East | West | South deriving (Eq)

instance Show Direction where
  show North = "North"
  show East = "East"
  show West = "West"
  show South = "South"

dirReduce :: [Direction] -> [Direction]
dirReduce ds
  | newDs == ds = ds
  | otherwise = dirReduce newDs
  where newDs = dirReduce' ds

dirReduce' :: [Direction] -> [Direction]
dirReduce' [] = []
dirReduce' [d] = [d]
dirReduce' (x:y:ds)
  | elem (x,y) [(North,South),(South,North),(East,West),(West,East)] = dirReduce' ds
  | otherwise = x:(dirReduce' (y:ds))
