import Text.Read (readMaybe)

safeHead  :: [a] -> Maybe a
safeHead [] = Nothing
safeHead list = Just (head list)


safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) i
    | length xs < i-1 = Nothing
    | i == 0 = Just x
    | otherwise = safeIndex xs (i-1)


-- DOROBIT 1.2 PO TOM CO DOROBIM CVICENI 4 

data Account = Account
  { username :: String
  , password :: String
  } deriving (Eq, Show)

data RegistrationError
  = EmptyUsername
  | PasswordTooShort Int
  | PasswordMismatch
  deriving (Eq, Show)


register :: String -> String -> String
         -> Either RegistrationError Account
register name pass pass2 
    | null name = Left EmptyUsername
    | length pass < 8 = Left (PasswordTooShort (length pass))
    | pass /= pass2 = Left PasswordMismatch 
    | otherwise = Right (Account name pass)


-- Definujte dátový typ pre reprezentáciu stromu aritmetických výrazov s operátormi +, - *, /, unárnym mínus, a číslami typu Double.
data Expr 
    = Mul Expr Expr
    | Del Expr Expr
    | Sub Expr Expr
    | Add Expr Expr
    | Num Double


myExp = Add (Mul (Num 1) (Num 2)) (Sub (Num 3) (Num 4))



-- Definujte funkciu pretty, ktorá vytvori textovú reprezentáciu výrazu (so zátvorkami).


pretty ::  Expr -> String
pretty (Mul a b) = "(" ++ pretty a ++ "*" ++ pretty b ++")"
pretty (Del a b) = "(" ++ pretty a ++ "/" ++ pretty b ++")"
pretty (Sub a b) = "(" ++ pretty a ++ "-" ++ pretty b ++")"
pretty (Add a b) = "(" ++ pretty a ++ "+" ++ pretty b ++")"
pretty (Num a) = show a



eval :: Expr -> Double
eval (Mul a b) = (eval a) * (eval b)  
eval (Del a b) = (eval a) / (eval b)  
eval (Sub a b) = (eval a) - (eval b)  
eval (Add a b) = (eval a) + (eval b)  
eval (Num a) = a 

-- Definujte typ pre binárny strom, kde každý list stromu môže obsahovať hodnotu ľubovoľného typu. Ten je ale rovnaký pre všetky listy.
data BTree a 
    = Leaf a 
    | Branch (BTree a)  (BTree a)

myLittleTree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)

leafCount :: BTree a -> Int 
leafCount (Leaf a) = 1
leafCount(Branch l r ) = leafCount(l) + leafCount(r)

contains  :: Eq a => a -> BTree a -> Bool
contains a (Branch l r) = contains a l || contains a r
contains a (Leaf b)
    | a == b = True
    | otherwise = False


mapBTree   :: (a -> b) -> BTree a -> BTree b
mapBTree f (Leaf a) = Leaf (f a)
mapBTree f (Branch l r) = Branch (mapBTree f l) (mapBTree f r)


sumBTree :: BTree Integer -> Integer
sumBTree (Leaf i) = i
sumBTree (Branch l r) = (sumBTree l) + (sumBTree r)


-- Definujte funkciu foldBtree, ktorá používa zadanú binárnu funkciu na výpočet hodnoty z údajov uložených v strome:


foldBTree :: (a -> a -> a) -> BTree a -> a
foldBTree f (Leaf a) = a
foldBTree f (Branch l r) = f (foldBTree f l)(foldBTree f r)

------------------6-------------------
allEqual:: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:xs) 
    | x == head xs = allEqual(xs)
    | otherwise = False

data Point = Point Double Double
    deriving (Eq, Show)

data Vector = Vec Double Double
    deriving (Eq, Show)

data Shape
    = Circle Point Double
    | Rectangle Point Point
    -- deriving (Eq, Show)


-- instance Eq Shape where
--     -- Circle a b == Circle c d = a == c && d == b
--     Rectangle x y == Rectangle a b = (x == b && y == a ) || (x == a && y == b)
--     -- _ == _ = False


class Movable a where
    move :: Vector ->  a -> a 
    reflectX :: a -> a 

instance Movable Point where 
    move (Vec x y)(Point z w) = Point (x+z)(y+w) 
    reflectX (Point z w) = Point (-z) w


instance Movable Shape where 
    move (Vec x y)(Circle (Point x' y') r)=  Circle (Point (x'+x)(y' +y))r
    move (Vec f g) (Rectangle (Point x y)(Point z w)) = Rectangle (Point (x+f) (y+g)) (Point (z+f)(w+g))
    reflectX(Circle(Point x y )r) = Circle (Point(-x) y) r 
    reflectX(Rectangle (Point x y)(Point z w)) = (Rectangle (Point (-x) y)(Point (-z) w)) 


instance Functor BTree where 
    fmap f (Branch l r) = Branch (fmap f l)(fmap f r)
    fmap f (Leaf a) = Leaf(f a)








-- Definujte inštanciu triedy Foldable pre typ BTree. V rámci inštancie implementujte funkciu foldr.

instance Foldable BTree where
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l
    foldr f acc (Leaf a) = f a acc

    foldl f acc (Branch l r) = foldl f (foldl f acc l) r
    foldl f acc (Leaf a) = f acc a


-----------------7-----------------------
-- Spustiteľný program v jazyku Haskell musí obsahovať funkciu main :: IO ().

-- Úloha 1.1
-- Vytvorte nový súbor hello.hs, obsahujúci funkciu main:
main :: IO ()
main = otherway
    -- do
    -- putStrLn " meno :"
    -- name <- getLine
    -- putStrLn "from"
    -- city<- getLine
    -- let response = "Hello " ++ name ++ " from " ++ city
    -- putStrLn response

    
otherway :: IO ()
otherway = 
    putStrLn "meno ">>
    getLine >>= \name ->
        putStrLn "mesto ">>
        getLine >>= \city ->
            let response = "hello "++ name ++ " from " ++ city in putStrLn response



getInt :: IO Int
getInt = 
    putStrLn "cislo:" >>
    getLine >>= \num ->
        case readMaybe num of 
            Nothing -> putStrLn "neplatny vstup ">> getInt
            Just num -> pure num



questions :: [String]
questions =
    [ "What is your name?"
    , "What is your favourite language?"
    , "What would you like to build?"
    ]

ask :: String -> IO String
ask q =
    putStrLn q >>
    getLine 
    -- getLine >>= \ans -> pure ans 


askAll :: [String] -> IO [String]
askAll [] = pure []
askAll (q:qs) =
    putStrLn q >> 
    getLine >>= \a -> 
        askAll qs >>= \ans ->
            pure (a:ans)

askAll2 :: [String] -> IO [String]
askAll2 qs = 
    mapM ask qs

-- DOROBIT 5 6 z 7