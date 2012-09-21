--25.6.2012
--haskell 2

data NTree a = NTree a [NTree a]
	deriving (Show)

sort :: (k->k->Bool) -> NTree (k,h) -> NTree (k, h)
sort cmp (NTree a trees) = NTree a (map (sort cmp) (qs cmp trees)) where
	qs :: (k->k->Bool) -> [NTree (k,h)] -> [NTree (k,h)]
	qs _ [] = []
	qs cmp (x:xs) = qs cmp [y| y <- xs, cmp (getKey y) (getKey x)] ++ [x] ++ qs cmp [y|y<-xs,not $ cmp (getKey y) (getKey x)]
	getKey (NTree (key,val) xs) = key
