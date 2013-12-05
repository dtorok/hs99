import Prelude hiding (last, length, reverse)

-- datatype for nested lists
data NL a = NLI a | NLL [(NL a)] -- NL: NestedList  NLI: NestedListItem  NLL: NestedListList


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
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = isPalindrome' xs (const True) where
	isPalindrome' [] f = f xs
	isPalindrome' (x:xs) f = isPalindrome' xs chk where
		chk (ox:oxs) = (ox == x) && (f oxs)

-- p7
flatten :: [[a]] -> [a]
flatten = flatten' [] where
	flatten' :: [a] -> [[a]] -> [a]
	flatten' res [] = res
	flatten' res (xs:xss) = flatten' (res ++ xs) xss

flattenExt :: [NL a] -> [a]
flattenExt = reverse . flattenExt' [] where
	flattenExt' :: [a] -> [NL a] -> [a]
	flattenExt' res [] = res
	flattenExt' res ((NLI a) : nl) = flattenExt' (a:res) nl
	flattenExt' res ((NLL as) : nl) = flattenExt' (flattenExt' res as) nl

flattenExt_v2 :: [NL a] -> [a]
flattenExt_v2 [] = []
flattenExt_v2 ((NLI a) : nl) = a : (flattenExt_v2 nl)
flattenExt_v2 ((NLL as) : nl) = flattenExt_v2 as ++ flattenExt_v2 nl


-- p8
compress :: Eq a => [a] -> [a]
compress = compress' Nothing where
	compress' :: Eq a => (Maybe a) -> [a] -> [a]
	compress' _ [] = []
	compress' Nothing (x:xs) = x : (compress' (Just x) xs)
	compress' (Just lastx) (x:xs) = if lastx == x then c else (x:c) where c = compress' (Just x) xs


-- helper
packGeneric :: Eq a => (a -> Int -> x) -> Maybe (a, Int) -> [a] -> [x]
packGeneric _ Nothing [] = []
packGeneric f (Just (x, cnt)) [] = [f x cnt]
packGeneric f Nothing (x:xs) = packGeneric f (Just (x, 1)) xs
packGeneric f (Just (lastx, cnt)) (x:xs) = if lastx == x 
												then                 packGeneric f (Just (lastx, cnt + 1)) xs
												else (f lastx cnt) : packGeneric f (Just (x, 1)) xs
-- p9
pack :: Eq a => [a] -> [[a]]
pack = packGeneric repeater Nothing where
	repeater x cnt = (take cnt . repeat) x


-- p10
encode :: Eq a => [a] -> [(Int, a)]
encode = packGeneric encoder Nothing where
	encoder x cnt = (cnt, x)
									

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
	check (isPalindrome [1, 1, 2, 3, 5, 8]) False
	check (isPalindrome [1, 2, 3, 2, 1]) True
	check (flatten [[1, 1], [2], [3, 5, 8]]) [1, 1, 2, 3, 5, 8]
	check (flattenExt [NLL [NLL [NLI 1, NLL [], NLI 1], NLI 2, NLL [NLI 3, NLL [NLI 5, NLI 8, NLL []]]]]) [1, 1, 2, 3, 5, 8]
	check (flattenExt_v2 [NLL [NLL [NLI 1, NLI 1], NLI 2, NLL [NLI 3, NLL [NLI 5, NLI 8]]]]) [1, 1, 2, 3, 5, 8]
	check (compress ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) ['a', 'b', 'c', 'a', 'd', 'e']
	check (compress ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e', 'f']) ['a', 'b', 'c', 'a', 'd', 'e', 'f']
	check (compress [] :: [Int]) []
	check (pack ['a', 'a', 'a', 'a', 'b', 'b', 'c', 'd', 'd', 'd', 'd', 'd', 'e']) [['a', 'a', 'a', 'a'], ['b', 'b'], ['c'], ['d', 'd', 'd', 'd', 'd'], ['e']]
	check (encode ['a', 'a', 'a', 'a', 'b', 'b', 'c', 'd', 'd', 'd', 'd', 'd', 'e']) [(4, 'a'), (2, 'b'), (1, 'c'), (5, 'd'), (1, 'e')]
	putStr " done\n"