--12.9.2012
--haskell 2

data BTree a = Nil | Vrchol (BTree a) a (BTree a)

fold :: b -> (b -> a -> b -> b) -> BTree a -> b

fold x _ Nil = x
fold x f (Vrchol left val right) = f (fold x f left) val (fold x f right)

vyska :: BTree a -> Int
vyska tree = fold 0 f tree where
	f :: Int -> a -> Int -> Int
	f left _ right = 1 + maximum [left,right]

listy :: BTree a -> Int
listy tree = fold 0 f tree where
	f :: Int -> a -> Int -> Int
	f left _ right = if left == 0 && right == 0 then 1 else left + right
