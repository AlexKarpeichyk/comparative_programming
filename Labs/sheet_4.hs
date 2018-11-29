-- Lab Sheet 4

-- 1. Types
{-
Prelude> :t (*)
(*) :: Num a => a -> a -> a
Prelude> :t (&&) True
(&&) True :: Bool -> Bool
Prelude> :t \x -> \f -> (f x)
\x -> \f -> (f x) :: r1 -> (r1 -> r) -> r
Prelude> :t tail [1, 2, 3]
tail [1, 2, 3] :: Num a => [a]
Prelude> :t error
error :: [Char] -> a
Prelude> :t \(x, y) -> x && True
\(x, y) -> x && True :: (Bool, t) -> Bool
-}

-- 2. Lists and Pattern Matching 

equal :: Eq a => [a] -> [a] -> Bool
equal [] [] = True
equal (h:t) (xh:xt) = (h == xh) && (equal t xt)

rev [] = []
rev [x] = [x]
rev (h:t) = (rev t) ++ [h]

palindrome [] = True
palindrome [x] = True
palindrome (h:t) = equal (rev (h:t)) (h:t)

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree f EmptyTree = EmptyTree
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)

addOne x = x + 1
 
