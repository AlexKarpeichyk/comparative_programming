--Lecture tasks

getSnd_1 = snd(fst((True, 'c'), 42))
getSnd (_, x, _, _) = x
getSnd_2 = getSnd(True, 'c', 42, (3, 4))

len [] = 0
len (h:t) = 1 + len t
hd [] = error "empty list"
hd (h:t) = h
tl [] = error "empty list"
tl (h:t) = t
app [] x = x
app (h:t) x = h:(app t x)

sum_l_1 x = if null x then 0 else head x + sum_l_1 (tail x)
sum_l_2 x 
	| null x = 0
	| otherwise = head x + sum_l_2 (tail x)
sum_l_3 [] = 0
sum_l_3 (h:t) = h + sum t

-- Lab Sheet 2

-- 1. Function Arguments

f x y = x + y
g (x, y) = x + y

sum_1 x y z = x + y + z
sum_2 (x, y, z) = x + y + z
sum_3 (x, y) z = x + y + z
sum_4 x (y, z) = x + y + z

fun a b c d = a * (b + b) - c

-- 2. Lists

inorder [] = True
inorder [x] = True
inorder (h:t)
	| h < head t = inorder t
	| otherwise = False

insert_0 x [] = [x]	
insert_0 x (h:t) 
	| h <= x && head t >= x = h:(x:t) 
	| otherwise = h:(insert_0 x t)

insert x [] = [x]
insert x (h:t)
	| x < h = x:(h:t)
	| otherwise = h:(insert x t)

sort [] = []
sort (h:t) = insert h (sort t)

-- 3. Trees

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeToList :: BinaryTree a -> [a]
treeToList EmptyTree = []
treeToList (Node v l r) = treeToList l ++ (v:(treeToList r))

sortedTree (Node v l r) 
	| inorder (treeToList (Node v l r)) = True
	| otherwise = False

sorted (Node v l r) = inorder . treeToList

insertNode x EmptyTree = Node x (EmptyTree) (EmptyTree)
insertNode x (Node v l r)
	| x > v = Node x (Node v (l) (r)) (EmptyTree)
	| otherwise = Node v (insertNode x (l)) (r)

-- 4. Additional Questions

preOrderTree EmptyTree = []
preOrderTree (Node v l r) = [v] ++ (preOrderTree l) ++ (preOrderTree r)

postOrderTree EmptyTree = []
postOrderTree (Node v l r) = (postOrderTree l) ++ (postOrderTree r) ++ [v]

revList [x] = [x]
revList (h:t) = (revList t) ++ [h] 

revTree EmptyTree = EmptyTree
revTree (Node v l r) = Node v (revTree r) (revTree l)
