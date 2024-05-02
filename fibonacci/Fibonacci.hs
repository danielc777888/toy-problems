
main :: IO ()
main = do
  print $ fib 20
  print $ fst (fib2 20)
  print $ fib3 20 (0, 1)

-- thinking functionally with haskell (Bird), pg. 164
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- thinking functionally with haskell (Bird), pg. 164
-- tupling
fib2 :: Int -> (Integer, Integer)
fib2 0 = (0, 1)
fib2 n = (b, a+b) where (a, b) = fib2 (n-1)

fib3 :: Int -> (Integer, Integer) -> Integer
fib3 0 (x, _) = x
fib3 n (x, y) = fib3 (n-1) (y, x+y)
