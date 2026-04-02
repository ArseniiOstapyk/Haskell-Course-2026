data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

data Token = TNum Int | TAdd | TSub | TMul | TDiv

-- 1)
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2)
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = foldr (\_ acc -> acc + 1) 0

-- 3)
instance Semigroup (Sequence a) where
    Empty <> s = s
    s <> Empty = s
    s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
    mempty = Empty

-- 4)
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x s = go [s]
  where
    go [] = False
    go (Empty:xs) = go xs
    go (Single y:xs) = y == x || go xs
    go (Append l r:xs) = go (l:r:xs)

-- 5)
tailToList :: Sequence a -> [a]
tailToList s = reverse (go [s] [])
  where
    go [] acc = acc
    go (Empty:xs) acc = go xs acc
    go (Single y:xs) acc = go xs (y:acc)
    go (Append l r:xs) acc = go (l:r:xs) acc

-- 6)
tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [x] = Just x
    go [] _ = Nothing
    go (TNum n:ts) stack = go ts (n:stack)
    go (TAdd:ts) (x:y:stack) = go ts ((y + x):stack)
    go (TSub:ts) (x:y:stack) = go ts ((y - x):stack)
    go (TMul:ts) (x:y:stack) = go ts ((y * x):stack)
    go (TDiv:ts) (0:_:_) = Nothing
    go (TDiv:ts) (x:y:stack) = go ts ((y `div` x):stack)
    go (_:_) _ = Nothing

-- 7)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- 8)
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n):ys)
        | x == y = (y, n + 1) : ys
        | otherwise = (x, 1) : (y, n) : ys

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []