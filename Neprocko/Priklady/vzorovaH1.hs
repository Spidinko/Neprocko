--Vzorova pisomka 
--Haskell 1
data Bag a = Item a | Items [Bag a]

fold :: (a->b) -> ([b]->b) -> Bag a -> b

fold functionForItem _ (Item a) = functionForItem a
fold functionForItem functionForList (Items bags) = let tbags = map (fold functionForItem functionForList) bags in
	functionForList tbags

listy :: Bag a -> [a]
listy = fold (\x->[x]) (foldr (++) [])
