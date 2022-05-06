type Element = (Int,Int)
type BinaryRelation = Element -> Bool
bound = 99

-- returns an empty binary relation, i.e. no pairs
emptyBinaryRelation :: BinaryRelation
emptyBinaryRelation = \_ -> False


-- returns the universal binary relation, i.e. all pairs
universalBinaryRelation :: BinaryRelation
universalBinaryRelation = \_ -> True


-- returns True of element is in binary relation; False otherwise
contains :: Element -> BinaryRelation -> Bool
contains e r = r e


-- returns a new binary relation obtained by adding element to binary relation; 
-- if element is already in binary relation then returns same binary relation
add :: Element -> BinaryRelation -> BinaryRelation
add e r = if r e then r else (\k -> e == k || r k)


-- returns new binary relation obtained by adding all elements in input list to
-- given binary relation.
addMultiple :: [Element] -> BinaryRelation -> BinaryRelation
addMultiple l r = foldr (\x acc -> add x acc) r l
-- Alternative -------------------------------
-- addMultiple [] r = r
-- addMultiple (e:es) r = add e (addMultiple es r)

-- Helper for equal
-- returns True if binary relation 1 is a subset of binary relation 2;
-- False otherwise.
subRelation :: BinaryRelation -> BinaryRelation -> Bool
subRelation r1 r2 = subRelationHelper1 r1 r2 0


subRelationHelper1 :: BinaryRelation -> BinaryRelation -> Int -> Bool
subRelationHelper1 r1 r2 n 
    | n > bound = True
    | otherwise = subRelationHelper2 r1 r2 n 0 && subRelationHelper1 r1 r2 (n+1)


subRelationHelper2 :: BinaryRelation -> BinaryRelation -> Int -> Int-> Bool
subRelationHelper2 r1 r2 n m 
    | m > bound                                    = True
    | contains (n,m) r1 && not (contains (n,m) r2) = False
    | otherwise                                    = subRelationHelper2 r1 r2 n (m+1)


-- returns True if binary relation 1 and binary relation 2 are equal;
-- False otherwise.
equal :: BinaryRelation -> BinaryRelation -> Bool
equal r1 r2 = and [r1 (x, y) && r2 (x, y)| x <- [0..bound], y <- [0..bound], r1 (x,y) || r2 (x,y)]
-- equal r1 r2 = (subRelation r1 r2) `&&` (subRelation r2 r1)
-- equal r1 r2 = foldr (&&) True [r1 (x, y) && r2 (x, y) | x <- [0..bound], y <- [0..bound], r1 (x, y) || r2 (x,y)]
-- equal r1 r2 = foldr (\x acc -> r1 x && r2 x) True [(x,y) | x <- [0..bound], y <- [0..bound], r1 (x, y) || r2 (x,y)]

-- universal set U = {0..99}; therefore, reflexive set should be {(0,0), (1,1),..., (99,99)}
-- for all a, (a,a) is in binary relation
reflexive :: BinaryRelation -> Bool
reflexive r = foldr (\e acc -> r (e,e) && acc) True [0..bound]
-- reflexive r =  and [r (x, x) | x <- [0..bound]]


-- returns the union of the two input binary relations
union :: BinaryRelation -> BinaryRelation -> BinaryRelation
-- Cheap Trick, but not Efficient in a long run: 
union r1 r2 = (\x -> r1 x || r2 x)
-- Slower Implementations
-- union r1 r2 = addMultiple [(x, y) | x <- [0..bound], y <- [0..bound], r1 (x, y) || r2 (x,y)] emptyBinaryRelation
-- union r1 r2 = foldr (\e acc -> add e acc) r1 [(x,y) | x <- [0..bound], y <- [0..bound], r2 (x,y)]  


-- returns inverse of input binary relation; 
-- inverse(r) = { (b,a) | (a,b) in r }
inverse :: BinaryRelation -> BinaryRelation
inverse r = addMultiple [(y, x) | x <- [0..bound], y <- [0..bound], r (x, y)] emptyBinaryRelation
-- inverse r = foldr (\(x,y) acc -> add (y,x) acc) emptyBinaryRelation [(x,y) | x <- [0..bound], y <- [0..bound], r (x,y)]


-- If (a,b) is in set S then there must be (b, a) in set S
-- returns True if input binary relation is symmetric; False otherwise
symmetric :: BinaryRelation -> Bool
symmetric r = and [r (y, x) | x <- [0..bound], y <- [0..bound], r (x, y)]
-- symmetric r = foldr (&&) True [r (y, x) | x <- [0..bound], y <- [0..bound], r (x, y)]


-- returns True if input binary relation is anti-symmetric; False otherwise
antiSymmetric :: BinaryRelation -> Bool
antiSymmetric r = and [not (r (y, x)) | x <- [0..bound], y <- [0..bound], r (x, y), x /= y]
-- antiSymmetric r = foldr (&&) True [not (r (y, x)) | x <- [0..bound], y <- [0..bound], r (x, y), x /= y]


-- returns True if input binary relation is transitive; False otherwise
transitive :: BinaryRelation -> Bool
transitive r = and [r (x, z) | x <- [0..bound], y <- [0..bound], z <- [0..bound], r (x, y) && r (y, z)]
-- transitive r = foldr (&&) True [r (x, z) | x <- [0..bound], y <- [0..bound], z <- [0..bound], r (x, y) && r (y, z), x /= y || y /= z ]


-- returns True if input binary relation is an equivalence; False otherwise
equivalence :: BinaryRelation -> Bool
equivalence r = reflexive r && symmetric r && transitive r


-- convert's a relation to its smallest reflexive form 
reflexiveClosure :: BinaryRelation -> BinaryRelation
reflexiveClosure r = addMultiple [(x,x) | x <- [0..bound], not (r (x, x))] r

-- convert's a relation to its smallest symmetric 
symmetricClosure :: BinaryRelation -> BinaryRelation
symmetricClosure r = addMultiple [(y, x) | x <- [0..bound], y <- [0..bound], r (x, y) && not (r (y, x)) && x /= y] r


-- selfJoin will be useful in implementing transitiveClosure;
-- returns selfJoin(r) = { (a,c) | (a,b) is in r and (b,c) is in r }
selfJoin :: BinaryRelation -> BinaryRelation
selfJoin r = addMultiple [(a,c) | a <- [0..bound], b <- [0..bound], c <- [0..bound], r (a,b) && r (b, c)] r
-- Alternative form ------------------------------------------------------------------------------------------------------
    -- let l = [(a, c) | a <- [0..bound], b <- [0..bound], c <- [0..bound], r (a, b) && r (b, c)]
    --     join r [] = r
    --     join r (x:xs) = join (add x r) xs
    -- in join r l
--------------------------------------------------------------------------------------------------------
-- Alternative idea:
-- selfJoin r = addMultiple [(a, c) | a <- [0..bound], b <- [0..bound], c <- [0..bound], r (a, b) && r (b, c)] emptyBinaryRelation


-- look up definition in BinaryRelationNotes.pdf
transitiveClosure :: BinaryRelation -> BinaryRelation
transitiveClosure r
    | equal r (selfJoin r) = r
    | otherwise = transitiveClosure (selfJoin r)
-- Alternative  form ----------------------------------------------------------------------------------------------
-- transitiveClosure r =
--     let store = selfJoin r
--         apply r'
--             | equal r' store = r'
--             | otherwise = transitiveClosure store
--     in apply r
-------------------------------------------------------------------------------------------------------------
-- Alternative idea: 
-- transitiveClosure r
--     | equal (union (selfJoin r) r) r = r
--     | otherwise = transitiveClosure (union (selfJoin r) r) 

-- returns String version of binary relation
toString :: BinaryRelation -> String
toString r = show [(x,y) | x <- [0..bound], y <- [0..bound], r (x,y)]