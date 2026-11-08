safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _     = Nothing
safeIndex (x:xs) i
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = safeIndex xs (i - 1)

data Book = Book
  { title    :: String
  , borrower :: Maybe String  
  } deriving (Eq, Show)

findBook :: String -> [Book] -> Maybe Book
findBook _ [] = Nothing
findBook searchTitle (b:bs)
  | title b == searchTitle = Just b
  | otherwise              = findBook searchTitle bs

borrowerOf :: String -> [Book] -> Maybe String
borrowerOf searchTitle books = extractBorrower (findBook searchTitle books)

-- Pomocná funkcia, ktorá rozoberá Maybe Book bez použitia case of
extractBorrower :: Maybe Book -> Maybe String
extractBorrower Nothing = Nothing                                    -- Kniha sa nenašla
extractBorrower (Just (Book _ _ Available)) = Nothing                 -- Kniha sa našla, ale nie je vypožičaná
extractBorrower (Just (Book _ _ (Borrowed name))) = Just name

data Account = Account
  { username :: String
  , password :: String
  } deriving (Eq, Show)

data RegistrationError
  = EmptyUsername
  | PasswordTooShort Int
  | PasswordMismatch
  deriving (Eq, Show)

register :: String -> String -> String -> Either RegistrationError Account
register u p pRepeat
  | null u          = Left EmptyUsername
  | length p < 8    = Left (PasswordTooShort 8)
  | p /= pRepeat    = Left PasswordMismatch
  | otherwise       = Right (Account { username = u, password = p })






  data Expr
  = Num Double          -- Číslo
  | Neg Expr            -- Unárne mínus (-x)
  | Add Expr Expr       -- Súčet (+)
  | Sub Expr Expr       -- Rozdiel (-)
  | Mul Expr Expr       -- Súčin (*)
  | Div Expr Expr       -- Podiel (/)
  deriving (Eq, Show)

-- -- Príklad z textu zadania: ((1 * 2) + (3 - 4))
-- myExp :: Expr
-- myExp = Add (Mul (Num 1) (Num 2)) (Sub (Num 3) (Num 4))





-- Pomocná funkcia na pekné zobrazenie reálneho čísla (napr. 1.0 -> "1", 2.5 -> "2.5")
showNum :: Double -> String
showNum x
  | x == fromInteger (round x) = show (round x :: Integer)
  | otherwise                 = show x

pretty :: Expr -> String
pretty (Num n)     = showNum n
pretty (Neg e)     = "(-" ++ pretty e ++ ")"
pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
pretty (Sub e1 e2) = "(" ++ pretty e1 ++ " - " ++ pretty e2 ++ ")"
pretty (Mul e1 e2) = "(" ++ pretty e1 ++ " * " ++ pretty e2 ++ ")"
pretty (Div e1 e2) = "(" ++ pretty e1 ++ " / " ++ pretty e2 ++ ")"


eval :: Expr -> Double
eval (Num n)     = n
eval (Neg e)     = - eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2



data BTree a
  = Leaf a
  | Branch (BTree a) (BTree a)
  deriving (Eq, Show)

-- Príklad z textu zadania:
myLittleTree :: BTree Integer
myLittleTree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)




-- Vráti počet listov v strome
leafCount :: BTree a -> Int
leafCount (Leaf _)     = 1
leafCount (Branch l r) = leafCount l + leafCount r

-- Zistí, či sa daná hodnota nachádza v niektorom z listov
contains :: Eq a => a -> BTree a -> Bool
contains x (Leaf v)     = x == v
contains x (Branch l r) = contains x l || contains x r

-- Aplikuje funkciu na hodnotu v každom liste
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f (Leaf v)     = Leaf (f v)
mapTree f (Branch l r) = Branch (mapTree f l) (mapTree f r)

foldBTree :: (a -> a -> a) -> BTree a -> a
foldBTree _ (Leaf v)     = v
foldBTree f (Branch l r) = f (foldBTree f l) (foldBTree f r)

