func a b = a + b

factorial :: Integer -> Integer

factorial x
    | x == 0 = 1
    | x == 1 = 1
    | x >1 = x * factorial (x-1)

    