--12.9.2012
--haskell 1

selections [] = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ (y:zs) | (y,ys) <- selections xs, zs <- permutations ys ]


perm :: Eq a => [a] -> [(a,a)] -> [[a]]
perm xs guards = filter (f guards) (permutations xs) where
	f :: Eq a => [(a,a)] -> [a] -> Bool
	f [] _ = True
	f ((a,b):ys) xs = (elemIndex a xs < elemIndex b xs) && f ys xs
	elemIndex :: Eq a => a -> [a] -> Int
	elemIndex _ [] = -1
	elemIndex y (x:xs) 
		| y == x = 1
		| otherwise = 1 + elemIndex y xs
