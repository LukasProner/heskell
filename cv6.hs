allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (y:x:xs)
    | x == y = allEqual (x:xs)
    | otherwise = False


data Point = Point Double Double
           deriving (Eq, Show)

data Vector = Vec Double Double
    deriving (Eq, Show)

data Shape = Circle Point Double
           | Rectangle Point Point
           deriving Show


instance Eq Shape where 
    Circle c1 r1 ==Circle c2 r2        =  c1 == c2 && r1 == r2
    Rectangle a1 b1 == Rectangle a2 b2 =  (a1 == a2 && b2 == b1) || (a1 == b2 && a2 == b1) 
    _ == _ = False

-- instance Ord Shape where 
--     Circle c1 r1 <= Circle c2 r2
--         | c1/=c2  = c1<c2
--         |otherwise = r1 <= r2
-- DOKONCI NECHAPES TOMU


-----------------3------------

class Movable a where
    move :: Vector -> a -> a
    reflectX :: a -> a 
    reflectY :: a -> a 
    rotate180 :: a -> a 

instance Movable Point where
    move (Vec vx vy) (Point x y) = Point (x+vx)(y+vy)
    reflectX (Point x y) = Point x (-y)
    reflectY (Point x y) = Point (-x) y
    rotate180 (Point x y ) = Point (-x)(-y)

instance Movable Shape where
    move v (Circle x r) = Circle (move v x) r
    move v (Rectangle x y) = Rectangle (move v  x) (move v y)

    reflectX (Circle x y) = Circle (reflectX x) y
    reflectX (Rectangle x y) = Rectangle (reflectX x)(reflectX y)

    reflectY (Circle x y) = Circle (reflectY x) y
    reflectY (Rectangle x y) = Rectangle (reflectY x)(reflectY y)

    rotate180 (Circle x y) = Circle (rotate180 x) y
    rotate180 (Rectangle x y) = Rectangle (rotate180 x)(rotate180 y)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor 