-- Lab Sheet 8

-- 1. Imperative Language

type Name = String
type Memory = [(Name, Integer)]

update :: Name -> Integer -> Memory -> Memory
update n i [] = [(n, i)]
update n i (h:t)
	| fst h == n = (fst h, i):t
	| otherwise = h:(update n i t)

find :: Name -> Memory -> Integer 
find n [x]
	| fst x == n = snd x
	| otherwise = 0
find n (h:t) 
	| fst h == n = snd h
	| otherwise = find n t

data Aexp = Int Integer | Var Name | Add Aexp Aexp | Mult Aexp Aexp deriving (Show, Eq)
data Bexp = And Bexp Bexp | Eq Aexp Aexp | Less Aexp Aexp | Not Bexp deriving (Show, Eq)
data Comm = Assign Name Aexp | Seq Comm Comm | Cond Bexp Comm Comm | While Bexp Comm deriving (Show)

evalA :: Aexp -> Memory -> Integer
evalA (Add (Int l) (Int r)) m = l + r
evalA (Mult (Int l) (Int r)) m = l * r
evalA (Add (Var l) (Int r)) m = find l m + r
evalA (Mult (Var l) (Int r)) m = find l m * r
evalA (Add (Var l) (Var r)) m = find l m + find r m
evalA (Mult (Var l) (Var r)) m = find l m * find r m
evalA (Add l r) m = (evalA l m) + (evalA r m)
evalA (Mult l r) m = (evalA l m) * (evalA r m)
 
evalB :: Bexp -> Memory -> Bool
evalB (And l r) m = evalB l m && evalB r m
evalB (Eq (Var l) (Var r)) m = find l m == find r m 
evalB (Eq l r) m = evalA l m == evalA r m
evalB (Less (Var l) (Var r)) m = find l m < find r m
evalB (Less l r) m = evalA l m < evalA r m
evalB (Not b) m = not (evalB b m)

evalC :: Comm -> Memory -> Memory
evalC (Assign n (Int i)) m = update n i m
evalC (Assign n a) m = update n (evalA a m) m
evalC (Seq a b) m = evalC b (evalC a m)
evalC (Cond b t e) m
	| evalB b m == True = evalC t m
	| otherwise = evalC e m  
evalC (While b c) m 
	| evalB b m == True = evalC (While b c) (evalC c m) 
	| otherwise = m

-- 2. Functional Language

data Lam = V Name | Abs Name Lam | App Lam Lam deriving (Show)
type Set = [Name]

add :: Name -> Set -> Set
add n s = [n] ++ s

join :: Set -> Set -> Set
join s t = s ++ t

remove :: Name -> Set -> Set
remove n (h:t)
	| n == h = t
	| otherwise = h:(remove n t)

	
