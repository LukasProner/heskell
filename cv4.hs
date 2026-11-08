data Date = Date Int Int Int
            deriving Show

today = Date 2020 10 14

isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

isLastInMonth (Date y m d) 
    |m == 2 && isLeapYear y && d == 29 = True
    |elem m [1,3,5,7,8,10,12] && d == 31 = True
    | m == 2 && d == 28 = True
    |elem m [4,6,9,11] && d == 30 = True
    |otherwise = False

nextDay (Date y m d)
    | isLastInMonth (Date y m d) && m == 12= (Date (y+1) 1 1)
    | isLastInMonth (Date y m d) = (Date y (m+1) 1)
    | otherwise = (Date y m (d+1)) 

----------------2 uloha-------------------------
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


perimeter :: Shape -> Double
perimeter(Circle _ r ) = 2 * pi * r
perimeter(Rectangle(Point x1 y1)(Point x2 y2)) = (abs(x2-x1))*2 + (abs(y2-y1))*2


data Vector = Vec Double Double
    deriving (Eq, Show)

move :: Vector -> Shape -> Shape
move (Vec vx vy) (Circle (Point px py) r) = Circle (Point(px + vx) (py + vy)) r

move (Vec vx vy) (Rectangle (Point p1x p1y) (Point p2x p2y)) = Rectangle (Point (p1x + vx) (p1y + vy)) (Point (p2x + vx) (p2y + vy)) 





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
availableTitles books = [title b | b<- books, loanStatus b==Available]

borrowedBy      :: String -> [Book] -> [Book]
borrowedBy s books = [b |b<-books , loanStatus b == Borrowed s]

borrowBook      :: String -> Book -> Maybe Book
borrowBook person book 
    | loanStatus book == Available =Just( book {loanStatus = Borrowed person})
    |otherwise = Nothing

returnBook      :: Book -> Book
returnBook book = book{loanStatus= Available}











testBooks :: [Book]
testBooks = 
    [ Book "Clean Code" "Robert C. Martin" Available
    , Book "The Pragmatic Programmer" "Andrew Hunt" (Borrowed "Peter")
    , Book "Haskell Programming" "Christopher Allen" Available
    , Book "Design Patterns" "Erich Gamma" (Borrowed "Anna")
    , Book "Refactoring" "Martin Fowler" (Borrowed "Peter")
    ]