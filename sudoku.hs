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


find_possible ms i j = [x | x <- [1..9], x `notElem` filled]
    where
    row = ms !! i
    col = (transpose ms) !! j
    square = get_square ms (i `div` 3) (j `div` 3)
    filled = filter (>0) $ nub $ row ++ col ++ square

list_replace xs n el = take n xs ++ [el] ++ drop (n + 1) xs

board_replace ms i j el = take i ms ++ [new_row] ++ drop (i + 1) ms 
    where
    row = ms !! i
    new_row = list_replace row j el

at xs i j = (xs !! i) !! j


main = do input <- readFile "input.txt"
          let xs = map _int $ filter ((0<).length) $ split "\n\r " $ input
          let ms = make_list xs 9
          let ch = check ms
          let fu = find_possible ms 0 0
          let all_possible = [(find_possible ms i j, (i, j)) | i <- [0..8], j <- [0..8], at ms i j == 0]
          mapM print all_possible
          putStrLn $ show fu
          let new_b = board_replace ms 1 1 9
            
          mapM print ms
          putStrLn ""
          mapM print new_b

