-- from "Thinking Functionally with Haskell", Richard Bird, Ch. 5

import Data.List (sort, transpose)

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

main :: IO ()
main = do
  putStrLn "Solving sudoku puzzle..."
  putStrLn $ show nasty
  putStrLn "Thinking..."
  let sol = head (solve nasty)
  putStrLn $ show sol
  putStrLn "SOLVED!!!"

-- 0.2s
test :: Grid
test = [['0','0','4','0','0','5','7','0','0'],
        ['0','0','0','0','0','9','4','0','0'],
        ['3','6','0','0','0','0','0','0','8'],
        ['7','2','0','0','6','0','0','0','0'],
        ['0','0','0','4','0','2','0','0','0'],
        ['0','0','0','0','8','0','0','9','3'],
        ['4','0','0','0','0','0','0','5','6'],
        ['0','0','5','3','0','0','0','0','0'],
        ['0','0','6','1','0','0','9','0','0']]

--2m47s
nasty :: Grid
nasty = [['0','0','0','0','6','0','0','8','0'],
         ['0','2','0','0','0','0','0','0','0'],
         ['0','0','1','0','0','0','0','0','0'],
         ['0','7','0','0','0','0','1','0','2'],
         ['5','0','0','0','3','0','0','0','0'],
         ['0','0','0','0','0','0','4','0','0'],
         ['0','0','4','2','0','1','0','0','0'],
         ['3','0','0','7','0','0','6','0','0'],
         ['0','0','0','0','0','0','0','5','0']]

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = search . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

single :: [a] -> Bool
single [_] = True
single _   = False

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
  where fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove _ [x] = [x]
remove ds xs = filter (`notElem` ds) xs

nodups :: (Ord a) => [a] -> Bool
nodups []     = True
nodups xs = and (zipWith (/=) ys (tail ys))
  where ys = sort xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols []       = []
cols [xs]     = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
  map cols .
  group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs:group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

search :: Matrix [Digit] -> [Grid]
search cm
  | not (safe pm) = []
  | complete pm = [extract pm]
  | otherwise = concat (map search (expand1 pm))
  where pm = prune cm

extract :: Matrix [Digit] -> Grid
extract = map (map head)

safe :: Matrix [Digit] -> Bool
safe cm = all ok (rows cm) &&
  all ok (cols cm) &&
  all ok (boxs cm)

ok :: Ord a => [[a]] -> Bool
ok row = nodups [x | [x] <- row]

complete :: Matrix [Digit] -> Bool
complete = all (all single)

counts :: [[[a]]] -> [Int]
counts = filter (/= 1) . map length . concat

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rs = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
  where (rows1, row:rows2) = break (any smallest) rs
        (row1, cs:row2) = break smallest row
        smallest r = length r == n
        n = minimum (counts rs)

-- matrix operations
scalarMult :: Num a => [a] -> [a] -> a
scalarMult xs ys = sum (zipWith (*) xs ys)

addMult :: Num a => Matrix a -> Matrix a -> Matrix a
addMult = zipWith (zipWith (+))

matMult :: Num a => Matrix a -> Matrix a -> Matrix a
matMult ma mb = [map (scalarMult row) mbt | row <- ma]
  where mbt = transpose mb
