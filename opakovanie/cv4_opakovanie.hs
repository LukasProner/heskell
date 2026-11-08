isLeapYear :: Int -> Bool
isLeapYear x =  (mod x 4 == 0 && mod x 100 /= 0) || ( mod x 400 == 0) 

data Date = Date Int Int Int
            deriving Show

today = Date 2020 10 14

isLastInMonth :: Date -> Bool
isLastInMonth (Date y m d)
    | elem m [1,3,5,7,8,10,12] && d == 31 = True
    | isLeapYear y && m == 2 && d == 28 = True
    | m== 2 && d == 29 = True
    | elem m [2,4,6,9,11] && d == 30 = True
    | otherwise = False

nextDay :: Date -> Date
nextDay (Date y m d) 
    | isLastInMonth (Date y m d) = Date y (m+1) 1
    |otherwise = Date y m (d+1)

data Point = Point Double Double
           deriving (Eq, Show)

data Shape = Circle Point Double
           | Rectangle Point Point
           deriving Show

perimeter :: Shape -> Double
perimeter (Circle _ r) = 2 * 3.14 * r 
perimeter (Rectangle (Point x1 y1)(Point x2 y2)) = 2 * abs (x2 - x1) + 2 * abs (y2 - y1) 



data Vector = Vec Double Double
    deriving (Eq, Show)

move :: Vector -> Shape -> Shape
move (Vec vx vy) (Circle (Point x y) r) = Circle (Point (x + vx) (y + vy)) r 

-- chyba rectangle ale chapes ne? 
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
availableTitles books = [title x | x <- books, loanStatus x == Available]

borrowedBy:: String -> [Book] -> [Book]
borrowedBy person books = [x | x<- books, loanStatus x == Borrowed person]

borrowBook      :: String -> Book -> Maybe Book
borrowBook person book 
    | loanStatus book == Available = Just (book { loanStatus = Borrowed person} )
    | otherwise = Nothing  

returnBook      :: Book -> Book
returnBook book = book{loanStatus = Available}


findBook  :: String -> [Book] -> Maybe Book

findBook titleOfBook books = Just (head (filter (\x -> title x == titleOfBook ) books))


borrowerOf :: String -> [Book] -> Maybe String
borrowerOf titleOfBook books = case findBook titleOfBook books of 
    Nothing -> Nothing
    Just book-> case loanStatus book of
        Available -> Nothing
        Borrowed person -> Just person 