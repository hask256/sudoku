import Data.List

len = length

_int :: String -> Int
_int = read

make_list [] _ = []
make_list xs n = (take n xs) : make_list (drop n xs) n

split _  [] = [""]
split ds (x:xs)
    | x `elem` ds = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
    rest = split ds xs

check9 xs = len fs == (len $ nub fs)
    where
    fs = filter (>0) xs

allT = all (==True)

check ms = (allT $ map check9 ms) &&
           (allT $ map check9 $ transpose ms)  &&
           (allT $ map check9 $ get_all_squares ms)

get_square ms i j = foldl1 (++) $ map (take 3 . drop (j*3)) rs

    where
    rs = take 3 . drop (i*3) $ ms
   

get_all_squares ms = [(get_square ms i j) | i <- [0..2] , j <- [0..2]]


main = do input <- readFile "input.txt"
          let xs = map _int $ filter ((0<).length) $ split "\n\r " $ input
          let ms = make_list xs 9
          let ch = check ms
          putStrLn $ show ch
          putStrLn $ show ms

