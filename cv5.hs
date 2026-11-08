-- safeHead  :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead xs = Just (head xs)

-- safeIndex :: [a] -> Int -> Maybe a
-- safeIndex [] _ = Nothing
-- safeIndex (x:xs) i 
--     | i> length xs = Nothing
--     | i == 0= Just x
--     | otherwise = safeIndex(xs) (i-1)

-- data LoanStatus
--   = Available
--   | Borrowed String  -- meno osoby, ktorá si knihu požičala
--   deriving (Eq, Show)

-- data Book = Book
--   { title      :: String
--   , author     :: String
--   , loanStatus :: LoanStatus
--   } deriving (Eq, Show)


-- testBooks :: [Book]
-- testBooks = 
--     [ Book "Clean Code" "Robert C. Martin" Available
--     , Book "The Pragmatic Programmer" "Andrew Hunt" (Borrowed "Peter")
--     , Book "Haskell Programming" "Christopher Allen" Available
--     , Book "Design Patterns" "Erich Gamma" (Borrowed "Anna")
--     , Book "Refactoring" "Martin Fowler" (Borrowed "Peter")
--     ]


-- findBook  :: String -> [Book] -> Maybe Book
-- findBook _ [] = Nothing
-- findBook a (x:xs)
--     | title x == a = Just x
--     | otherwise = findBook a xs

-- borrowerOf :: String -> [Book] -> Maybe String
-- borrowBook _ [] = Nothing
-- borrowerOf a books = case findBook a books of 
--     Nothing -> Nothing
--     Just book -> case loanStatus book of 
--         Available -> Nothing
--         Borrowed person -> Just person

-- data Account = Account
--   { username :: String
--   , password :: String
--   } deriving (Eq, Show)

-- data RegistrationError
--   = EmptyUsername
--   | PasswordTooShort Int
--   | PasswordMismatch
--   deriving (Eq, Show)

-- register :: String -> String -> String -> Either RegistrationError Account
-- register u p pRepeat
--   | null u          = Left EmptyUsername
--   | length p < 8    = Left (PasswordTooShort 8)
--   | p /= pRepeat    = Left PasswordMismatch
--   | otherwise       = Right (Account { username = u, password = p })



safeHead  :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) i 
    | i> length xs = Nothing
    | i == 0= Just x
    | otherwise = safeIndex(xs) (i-1)

-- do  ulohy je v 04 opakovanie 
data Account = Account
  { username :: String
  , password :: String
  } deriving (Eq, Show)

data RegistrationError
  = EmptyUsername
  | PasswordTooShort Int
  | PasswordMismatch
  deriving (Eq, Show)

register :: String -> String -> String-> Either RegistrationError Account
register name pass pass2 
    | name == "" = Left EmptyUsername
    | length pass < 8 = Left (PasswordTooShort (length pass))
    | pass /= pass2 = Left PasswordMismatch 
    | otherwise = Right (Account name pass)

myExp = Add (Mul (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

data Expr
  = Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

pretty :: Expr -> String
pretty (Num n) =  show n
pretty (Add exp1 exp2) = "(" ++ pretty exp1++ "+"++ pretty exp2++ ")"
pretty (Mul exp1 exp2 ) = "(" ++pretty exp1++ "*" ++pretty exp2 ++")"
pretty (Sub exp1 exp2 ) = "(" ++pretty exp1++ "-"++ pretty exp2++ ")"
pretty (Div exp1 exp2 ) = "(" ++pretty exp1 ++"/" ++pretty exp2 ++")"


eval :: Expr -> Double
eval (Num n) = n
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2
eval (Sub exp1 exp2) = eval exp1 - eval exp2
eval (Div exp1 exp2) = eval exp1 / eval exp2

data BTree a 
  = Leaf a 
  | Branch (BTree a )(BTree a)
  deriving (Eq, Show)

myLittleTree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)

leafCount :: BTree a -> Int
leafCount (Leaf _ ) = 1
leafCount (Branch l r) = leafCount l + leafCount r

contains  :: Eq a => a -> BTree a -> Bool
contains a (Leaf b) 
  | a == b = True
  |otherwise = False

contains a (Branch l r) = contains a l || contains a r  


mapTree   :: (a -> b) -> BTree a -> BTree b
mapTree f (Leaf a ) = Leaf (f a)
mapTree f (Branch l r) = Branch (mapTree f l) (mapTree f r)  

sumBTree :: BTree Integer -> Integer
sumBTree (Leaf a) = a
sumBTree (Branch l r) = sumBTree l + sumBTree r

foldBTree :: (a -> a -> a) -> BTree a -> a
foldBTree _ (Leaf a) = a
foldBTree f (Branch l r) = f (foldBTree f l)(foldBTree f r) 


sumBTree2 :: Num a => BTree b -> b
sumBTree2 (Leaf a) = a
sumBTree2 (Branch l r) = sumBTree2 l + sumBTree2 