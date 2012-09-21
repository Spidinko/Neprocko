--18.6.2012
--haskell 1

cmp :: Ord a => [(Int,a)] -> [(Int,a)] -> Bool
cmp x y = let (sx,sy) = (qs x, qs y) in
	cmp' sx sy where
	cmp' _ [] = False
	cmp' [] _ = True
	cmp' ((i1,c1):xs) ((i2,c2):ys)
		| c2 > c1	= True
		| c2 < c1	= False
		| i1 == i2	= cmp' xs ys
		| i1 < i2	= cmp' xs (((i2-i1),c2):ys)
		| otherwise	= cmp' (((i1-i2),c1):xs) ys


qs :: Ord a => [(Int,a)] -> [(Int,a)]
qs [] = []
qs (x:xs) = qs [y|y<-xs, cm y x] ++ [x] ++ qs [y|y<-xs, not $ cm y x] where
	cm (_,x) (_,y) = x > y
