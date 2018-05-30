--Q6
solve :: [Int] -> Int -> [Expr]
solve ns n = [ e | x <- perms ns, e <- exprs x, eval e == n]

--Q1
data Expr = Val Int | App Op Expr Expr 
data Op = Add | Mul

eval :: Expr -> Int
eval (Val n) = n 
eval (App Add x y) = eval x + eval y
eval (App Mul x y) = eval x * eval y

values :: Expr -> [ Int ]
values (Val n) = [n]
values (App o x y) = values x ++ values y

--Q2
delete :: Int -> [ Int ] -> [ Int ]
delete n [] = []
delete n (x:xs) | n==x = xs
                | otherwise = x : delete n xs

--Q3
perms :: [Int] -> [[Int]]
perms [] = [[]]
perms [x] = [[x]]
perms xs = [y | x <- xs, y <- (map (x:)) (perms(delete x xs)) ]

--Q4
split :: [ Int ] -> [([Int], [Int])]
split [] = []
split [x] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs ]

--Q5 
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [ e | (lh, rh) <- split xs, l <- exprs lh, r <- exprs rh, e <- create l r ]  

create :: Expr -> Expr -> [Expr]
create l r  = [App o l r | o <- [Add,Mul]]

instance Show Expr where
    show (Val n) = show n
    show (App Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (App Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"