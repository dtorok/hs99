import Prelude hiding (last, length, reverse)

-- p1
last :: [a] -> a
last [] = undefined
last (x:[]) = x
last (x:xs) = last xs

-- p2
penultimate :: [a] -> a
penultimate [] = undefined
penultimate (x:[]) = undefined
penultimate (y:[x]) = y
penultimate (x:xs) = penultimate xs

-- p3
nth :: Int -> [a] -> a
nth 0 [] = undefined
nth 0 (x:xs) = x
nth i (x:xs) = nth (i-1) xs

-- p4
length :: [a] -> Int
length [] = 0
length (x:xs) = (length xs) + 1

-- p5
reverse :: [a] -> [a]
reverse = reverse' [] where
	reverse' rxs [] = rxs
	reverse' rxs (x:xs) = reverse' (x:rxs) xs

-- p6
--isPalindrom :: Eq a => [a] -> Bool
--isPalindrom 

check :: (Eq a, Show a) => a -> a -> IO ()
check a b = do
	if a == b then
		putStr "."
	else
		error $ "FAILURE: " ++ (show a) ++ " != " ++ (show b)

test :: IO ()
test = do
	putStr "Running tests: "
	check (last [1, 1, 2, 3, 5, 8]) 8
	check (penultimate [1, 1, 2, 3, 5, 8]) 5
	check (nth 2 [1, 1, 2, 3, 5, 8]) 2
	check (length [1, 1, 2, 3, 5, 8]) 6
	check (reverse [1, 1, 2, 3, 5, 8]) [8, 5, 3, 2, 1, 1]
	putStr " done\n"