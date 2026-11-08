data Date = Date Int Int Int
            deriving Show

today = Date 2020 10 14

isLeapYear :: Int -> Bool
isLeapYear x 
    | (x `mod` 4 ==0 && mod x 100 /= 0) || (x `mod`400==0) = True 
    |otherwise= False

isLastInMonth :: Date -> Bool
isLastInMonth (Date y m d)
    |((isLeapYear y && d == 29)||(m `elem` [1,3,5,7,8,10,12] && d == 31) || (m `elem` [2,4,6,9,11] && d == 30)|| (isLeapYear y ==False && d ==28)) = True
    |otherwise = False


data Point = Point Double Double
           deriving (Eq, Show)

data Shape = Circle Point Double
           | Rectangle Point Point
           deriving Show

smallCircle = Circle (Point 0 0) 1
largeCircle = Circle (Point 0 0) 10
someRectangle = Rectangle (Point 0 0) (Point 10 5)
aSquare = Rectangle (Point 0 0) (Point 10 10)
shapes = [smallCircle, largeCircle, someRectangle, aSquare]

area :: Shape -> Double
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2-x1)) * (abs (y2-y1))

perimeter :: Shape -> Double
perimeter (Circle _ r) = 2 * pi * r
perimeter (Rectangle (Point x1 y1)(Point x2 y2)) = abs((x1-x2)*2) + abs((y1-y2)*2)



data Vector = Vec Double Double
    deriving (Eq, Show)

move :: Vector -> Shape -> Shape
move (Vec v1 v2) (Circle (Point x1 x2) r) = (Circle (Point (x1+v1) (x2+v2)) r) 
move (Vec v1 v2) (Rectangle (Point x1 y1)(Point x2 y2)) = (Rectangle (Point (x1+v1) (y1+v2))(Point (x2+v1) (y2+v2)))




--------------------------------------------------
testBooks :: [Book]
testBooks = 
    [ Book "Clean Code" "Robert C. Martin" Available
    , Book "The Pragmatic Programmer" "Andrew Hunt" (Borrowed "Peter")
    , Book "Haskell Programming" "Christopher Allen" Available
    , Book "Design Patterns" "Erich Gamma" (Borrowed "Anna")
    , Book "Refactoring" "Martin Fowler" (Borrowed "Peter")
    ]


data LoanStatus
  = Available
  | Borrowed String  -- meno osoby, ktorá si knihu požičala
  deriving (Eq, Show)

data Book = Book
  { title      :: String
  , author     :: String
  , loanStatus :: LoanStatus
  } deriving (Eq, Show)

availableTitles :: [Book] -> [String]
availableTitles books = map (\y -> title y) (filter(\x -> loanStatus x == Available)books)

borrowedBy      :: String -> [Book] -> [Book]
borrowedBy person books = [ x | x<- books , loanStatus x == Borrowed person]

borrowBook      :: String -> Book -> Book
borrowBook person book 
    | loanStatus book == Available = book{loanStatus = Borrowed person}
    |otherwise = book 


returnBook      :: Book -> Book
returnBook book = book{loanStatus = Available}

borrowBook2      :: String -> Book -> Maybe Book
borrowBook2 person book 
    | loanStatus book == Available = Just (book{loanStatus = Borrowed person})
    |otherwise = Nothing 

------------CV 5 -----------------
safeHead  :: [a] -> Maybe a
safeIndex :: [a] -> Int -> Maybe a

safeHead list 
    | length(list) == 0 = Nothing
    |otherwise =Just(head list)

safeIndex [] _ = Nothing
safeIndex (x:xs) i
    | i > length xs = Nothing
    | i>0 = safeIndex (xs) (i-1)
    |otherwise = Just x


findBook  :: String -> [Book] -> Maybe Book
findBook name books = Just (head[x | x<- books , title x == name])
borrowerOf :: String -> [Book] -> Maybe String
borrowerOf tit books = case safeHead([x |x<-books, title x == tit]) of
    Nothing -> Nothing
    Just book -> case loanStatus book of 
        Available -> Nothing
        Borrowed name -> Just name


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
register name pass1 pass2 
    | null name = Left EmptyUsername
    | length pass1 < 8 = Left (PasswordTooShort (length pass1))
    | pass1 /= pass2 = Left PasswordMismatch
    | otherwise = Right (Account name pass1) 


---------------3.1 -----------
-- Definujte dátový typ pre reprezentáciu stromu aritmetických výrazov s operátormi +, - *, /, unárnym mínus, a číslami typu Double.
data Expr 
    = Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    | Num Double
-- deriving(Eq,Show)
-- Úloha 3.2
-- Definujte funkciu pretty, ktorá vytvori textovú reprezentáciu výrazu (so zátvorkami).

pretty :: Expr -> String
pretty (Add a b ) = "("++ pretty a ++"+"++pretty b ++")"  
pretty (Mul a b ) = "("++ pretty a ++"*"++pretty b++")"  
pretty (Div a b ) = "("++ pretty a++ "/"++pretty b++")"  
pretty (Sub a b ) = "("++ pretty a++ "-"++pretty b++")"  
pretty (Num i) = show i  

myExp = Add (Mul (Num 1) (Num 2)) (Sub (Num 3) (Num 4))

-- Definujte funkciu eval, ktorá vypočíta zadaný výraz.
eval :: Expr -> Double
eval (Num i) = i
eval (Add a b) =(eval a) + (eval b)  
eval (Div a b) =(eval a) / (eval b)  
eval (Mul a b) =(eval a) * (eval b)  
eval (Sub a b) =(eval a) - (eval b)  


-- Úloha 4.1
-- Definujte typ pre binárny strom, kde každý list stromu môže obsahovať hodnotu ľubovoľného typu. Ten je ale rovnaký pre všetky listy.

data BTree a 
    = Branch (BTree a)(BTree a)
    | Leaf a
    deriving(Eq,Show)

myLittleTree = Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)

-- Definujte funkcie pre prácu s takýmto stromom:
leafCount :: BTree a -> Int
leafCount (Branch l r) = (leafCount l) + (leafCount r)
leafCount (Leaf _ ) = 1 


contains  :: Eq a => a -> BTree a -> Bool
contains a (Branch l r) = (contains a l) || (contains a r) 
contains a (Leaf b) 
    | a == b = True
    |otherwise = False

mapBTree   :: (a -> b) -> BTree a -> BTree b
mapBTree f (Branch l r) = Branch(mapBTree f l)(mapBTree f r)
mapBTree f (Leaf a) = Leaf (f a) 


sumBTree :: BTree Integer -> Integer
sumBTree (Branch l r) = (sumBTree l)+(sumBTree r)
sumBTree (Leaf i) = i


-- Definujte funkciu foldBtree, ktorá používa zadanú binárnu funkciu na výpočet hodnoty z údajov uložených v strome:
foldBTree :: (a -> a -> a) -> BTree a -> a
foldBTree f (Branch l r) = f(foldBTree f l)(foldBTree f r)
foldBTree f(Leaf a )= a



----------------------cv 6 ------------------------

-- Implementujte funkciu allEqual, ktorá vráti True ak všetky hodnoty v zozname sú rovné, alebo ak je zoznam prázdny. Aký je typ tejto funkcie?

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) 
    | x == y = allEqual(y:xs) 
    |otherwise = False

-- Upravte definíciu funkcie sumBTree z minulého cvičenia tak, aby dokázala pracovať nielen so stromami obsahujúcimi Integer, ale aj iné numerické typy, napríklad Int alebo Double.

sumBTree2 ::Num a => BTree a -> a
sumBTree2 (Branch l r) = (sumBTree2 l)+(sumBTree2 r)
sumBTree2 (Leaf i) = i

-- Definujte inštanciu typovej triedy Eq pre typ Shape tak, aby obdĺžniky, ktoré sa líšia iba poradím bodov, ktoré ich definujú, boli považované za rovné.

instance Eq Shape where
    Circle a b == Circle c d = a == c && d == b
    Rectangle a b == Rectangle c d = (a==d && b==c) || (a== c && b == d)
    _ == _ = False 


-- Definujte typovú triedu Movable s týmito metódami:

-- move — posunie objekt (druhý argument) o zadaný vektor (prvý argument),
-- reflectX — zrkadlí objekt okolo súradnicovej osi X,
-- reflectY — zrkadlí objekt okolo súradnicovej osi Y,
-- rotate180 — otočí objekt o 180° okolo stredu súradnicovej sústavy.


class Movable a where
    move2 :: Vector -> a -> a
    reflectX :: a ->a
    reflectY :: a ->a
    rotate180 :: a ->a

-- Úloha 3.2
-- Definujte inštancie triedy Movable pre Point a Shape.
instance Movable Point where
    move2 (Vec v1 v2) (Point x y) = Point (x + v1)(y+v2)
    reflectX (Point x y) = Point (-x) y
    reflectY (Point x y) = Point x (-y)
    rotate180 (Point x y) = Point (-x)(-y)


instance Movable Shape where
    move2 (Vec v1 v2)(Circle (Point x y) r) = Circle (Point(x+v1)(y+v2))r
    move2(Vec v1 v2)(Rectangle (Point x1 y1)(Point x2 y2)) = Rectangle(Point (x1 + v1)(y1+v2))(Point (x2 + v1)(y2+v2))
    reflectX (Circle (Point x y) r) = Circle (Point (-x) y) r
    reflectX (Rectangle (Point x1 y1)(Point x2 y2)) = Rectangle (Point (-x1) y1)(Point (-x2) y2)
    reflectY (Circle (Point x y) r) = Circle (Point x (-y)) r
    reflectY (Rectangle (Point x1 y1)(Point x2 y2)) = Rectangle (Point x1 (-y1))(Point x2 (-y2))
    rotate180 (Circle (Point x y) r) = Circle (Point (-x) (-y)) r
    rotate180 (Rectangle (Point x1 y1)(Point x2 y2)) = Rectangle (Point (-x1) (-y1))(Point (-x2) (-y2))


-- Mnohé funkcie pre prácu s údajovými štruktúrami sú definované nielen pre zoznamy, ale aj pre iné štruktúry. Napríklad okrem funkcie map existuje jej všeobecnejšia forma fmap definovaná pre triedu Functor. Funkcie foldr, foldl a mnohé ďalšie tiež nie sú obmedzené iba pre zoznamu, ale sú definované pre triedu Foldable.

-- Úloha 4.1
-- Definujte inštanciu typovej triedy Functor pre typ stromu. Trieda Functor deklaruje funkciu:

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
-- Je to všeobecnejšia verzia funkcie map. V prípade stromu zmení vstupnou funkciou každý jeho prvok.

instance Functor BTree where
    fmap f (Branch a b) = Branch(fmap f a)(fmap f b)
    fmap f (Leaf a ) = Leaf(f a) 

instance Foldable BTree where
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l

---------------------------cv5 --------------------------------


-- hello.hs
