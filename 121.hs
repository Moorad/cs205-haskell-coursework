--  Group Name: CS-205 Assignment 121
--
--  Student Names:          Student ID:
--
--  Moorad Altamimi         854378
--  Thomas Ayling           2013325
--  DAUMANTAS BALAKAUSKAS   2015497
--
-- We, Tom and Moorad, both attempted to contact Daumantas, however we have had no response and as such he was not included in any of the work

import Data.List 

-- Question 1
-- Part I 

-- This function works by taking 3 numbers and filtering out any number that is bigger than the average.
-- After that we end up with a list of all the numbers below the average and then we take the "length" of it to get the final answer.

howManyBelowAverage :: Int -> Int -> Int -> Int
howManyBelowAverage x y z = length (filter (\n -> fromIntegral n < fromIntegral (x + y + z) / 3) [x, y, z])

-- A function to calculate an average from 3 ints
average :: Int -> Int -> Int -> Float
average x y z= (fromIntegral x + fromIntegral y + fromIntegral z ) / 3

-- An alternate function to calculate how many of 3 numbers is below the average of these numbers.
howManyBelowAverage2 :: Int -> Int -> Int -> Int
howManyBelowAverage2 x y z
    |average x y z > fromIntegral x && average x y z > fromIntegral y = 2
    |average x y z > fromIntegral x && average x y z > fromIntegral z = 2
    |average x y z > fromIntegral y && average x y z > fromIntegral z = 2
    |average x y z > fromIntegral x = 1
    |average x y z > fromIntegral y = 1
    |average x y z > fromIntegral z = 1
    |otherwise = 0

q1Test :: Int -> Int -> Int -> Bool
q1Test x y z = howManyBelowAverage x y z == howManyBelowAverage2 x y z

doq1Test :: Bool
doq1Test = q1Test 854378 2013325 2015497


-- Question 2

-- For this question I decided to split it into 3 different functions to make it easier to read and understand, one function for each calculation

-- Takes pizza size as input and calculates size cost with the formula pi * radius^2 * 0.0002
calculateSizeCost :: Float -> Float
calculateSizeCost x = pi * (x / 2) ** 2 * 0.0002

-- Takes pizza size and number of toppings as input and calculates topping cost with the formula pi * radius^2 * toppingCount * 0.001
calculateToppingCost :: Float -> Int -> Float
calculateToppingCost x y = pi * (x / 2) ** 2 * fromIntegral y * 0.001

-- Takes the number of sauces as input and calculates sauce cost with the formula sauceCount * 0.5
calculateSauceCost :: Int -> Float
calculateSauceCost x = fromIntegral x * 0.5


-- Uses the above functions to calculate the final cost with the 1.5 profit multiplier and truncates the final answer to two decimal places
pizzaPricing :: Float -> Int -> Int -> Float
pizzaPricing x y z = fromIntegral (truncate (100 * (calculateSizeCost x + calculateToppingCost x y + calculateSauceCost z) * 1.5)) / 100


-- Question 3

-- Part i
data Direction = North | West | South | East
   deriving (Show, Eq)

followDirection :: (Int, Int) -> Direction -> (Int,Int)
followDirection (x,y) d
  | d == North = (x,y + 1)
  | d == South = (x,y - 1)
  | d == East =  (x + 1,y)
  | d == West =  (x - 1,y)

-- Part ii

-- Recursively goes through all the directions in the list and applies followDirection function to each element.
followDirections :: (Int, Int) -> [Direction] -> (Int, Int)
followDirections (x, y) [] = (x, y)
followDirections (x, y) xs = followDirections (followDirection (x,y) (head xs)) (tail xs)
-- Part iii

data RelativeDirection = GoFoward | GoBack | GoLeft | GoRight
    deriving Show

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

-- Converts "Direction facing" and "Direction going" to a relative direction
directionsToRelative :: Direction -> Direction -> RelativeDirection
directionsToRelative x y
    | x == y = GoFoward
    | turnRight x == y = GoRight
    | turnLeft x == y = GoLeft
    | otherwise = GoBack

-- Recursively applies directionsToRelative to each step to convert a list of directions to relative directions.
relativizeDirections :: Direction -> [Direction] -> [RelativeDirection]
relativizeDirections x [] = []
relativizeDirections x xs = directionsToRelative x (head xs) : relativizeDirections (head xs) (tail xs)

-- Part iv

-- A function to convert a list of directions to coordiantes (x, y)
directionsToCoords :: (Int, Int) -> [Direction] -> [(Int, Int)]
directionsToCoords c [] = [c]
directionsToCoords c d = c : directionsToCoords (followDirection c (head d)) (tail d)

-- A function to convert two coordiantes to 1 Direction
coordsToDirection :: (Int, Int) -> (Int, Int) -> Direction
coordsToDirection (x1, y1) (x2, y2)
    | x2 - x1 == 0 && y2 - y1 == 1 = North
    | x2 - x1 == 1 && y2 - y1 == 0 = East
    | x2 - x1 == 0 && y2 - y1 == -1 = South
    | x2 - x1 == -1 && y2 - y1 == 0 = West

-- A function to convert a list of coordinates to a list of directions by applying coordsToDirection recursively
coordsToDirections :: [(Int, Int)] -> [Direction]
coordsToDirections c
    | null (tail c) = []
    | otherwise = coordsToDirection (head c) (head (tail c)) : coordsToDirections (tail c)

-- A function that takes a list of coordinates and removes all loops.
-- It does this by checking if there are any duplicate coordinates. If the same exact coordinate exists twice or more in the list
-- then a loop must have occured and it gets removed by finding the second occurance of the coordinate and remove everything in between
-- the first and the second coordinate.
removeLoops :: [(Int, Int)] -> [(Int, Int)]
removeLoops [] = []
removeLoops d = case head d `elemIndex` tail d of
        Just n -> head d : removeLoops (drop (n + 1) (tail d))
        Nothing -> head d : removeLoops (tail d)


-- The final function that applies everything together. It firstly converts the list of directions into coordinates,
-- remove all the loops and then converts the list back to directions
sanitizeDirections :: [Direction] -> [Direction]
sanitizeDirections xs = coordsToDirections (removeLoops (directionsToCoords (0,0) xs))

-- Question 4

data Orientation = H | V
    deriving (Show, Eq)
type Wall  = (Int, Int, Orientation)
type Maze = ((Int, Int), [Wall])

exampleMaze :: Maze
exampleMaze = ((4,4), hWalls ++ vWalls)
    where
        vWalls = map (\ (i,j) -> (i,j,V))
            [
            (0,0),(0,1),(0,2),(0,3),
            (1,1),(1,2),
            (2,1),(2,2),
            (3,2), (3,3),
            (4,0),(4,1),(4,2)
            ]
        hWalls = map (\ (i,j) -> (i,j,H))
            [
            (0,0),(1,0),(2,0),(3,0),
            (0,1), (2,1),
            (2,2),
            (0,4),(1,4),(2,4),(3,4)
            ]

exampleMaze2 :: Maze
exampleMaze2 = ((4,4), hWalls ++ vWalls)
    where
        vWalls = map (\ (i,j) -> (i,j,V))
            [
            (0,0),(0,1),(0,2),(0,3),
            (1,1),(1,2),
            (2,1),(2,2),
            (3,2), (3,3),
            (4,0),(4,1),(4,2),(4,3)
            ]
        hWalls = map (\ (i,j) -> (i,j,H))
            [
            (0,0),(1,0),(2,0),(3,0),
            (0,1), (2,1),
            (2,2),
            (0,4),(1,4),(2,4),(3,4)
            ]

-- This function checks for a given coordinate and facing direction if there is a wall to the left of the direction
-- we are facing or not.
checkWallLeft :: (Int, Int) -> Direction -> Maze -> Bool
checkWallLeft (x, y) North m = (x, y, V) `elem` snd m
checkWallLeft (x, y) East m = (x, y + 1, H) `elem` snd m
checkWallLeft (x, y) West m = (x, y, H) `elem` snd m
checkWallLeft (x, y) South m = (x + 1, y, V) `elem` snd m


-- Similar to checkWallLeft, this function checks for a given coordinate and facing direction if there is a wall ahead or not.
checkWallForward :: (Int, Int) -> Direction -> Maze -> Bool
checkWallForward (x, y) North m = (x, y + 1, H) `elem` snd m
checkWallForward (x, y) East m = (x + 1, y, V) `elem` snd m
checkWallForward (x, y) West m = (x, y, V) `elem` snd m
checkWallForward (x, y) South m = (x, y, H) `elem` snd m

-- This function takes a coordinate and a facing direction and moves the coordinate by one depending on the facing direction.
moveForward :: (Int, Int) -> Direction -> (Int, Int)
moveForward (x, y) North = (x, y + 1)
moveForward (x, y) East = (x + 1, y)
moveForward (x, y) West = (x - 1, y)
moveForward (x, y) South = (x, y - 1)

-- This function goes through the maze and traces until it finds the exit/find no exit, it does that by:
-- 1 - checking if the current square is outside the given size of the maze. If it is then it means we found the exit. (base case)
-- 2 - checking if we are at (0, 0) facing West. If we start at (0, 0) facing North then there is no way we get to (0,0) facing
-- West unless we loop around all possible squares. If that is the case then we return an empty list to stop the recursing.
-- 3 - we begin checking if there is a wall to the left of us. If there is then we check if there is a wall forward otherwise
-- we turn left, move forward once and call the function again with the our new position.
-- 4 - if there is a wall to the left of us and there is a wall in front of us then we turn right and call the function again (if its a dead end
-- then after recursing this step will be perfromed again making us turn right again i.e. face backwards) otherwise if there isnt a wall forward
-- then we move forward by one and call the function with the new position.
pathMaze :: (Int, Int) -> Direction -> Maze -> [Direction]
pathMaze (x, y) z m
  | fst (fst m) - 1 < x || snd (fst m) - 1 < y = []
  | x == 0 && y == 0 && z == West = []
  | checkWallLeft (x, y) z m = if not (checkWallForward (x, y) z m)
                             then z : pathMaze (moveForward (x, y) z) z m
                             else pathMaze (x, y) (turnRight z) m
  | otherwise = turnLeft z : pathMaze (moveForward (x, y) (turnLeft z)) (turnLeft z) m

-- We convert the output to Maybe [Direction] from [Direction] and use the followDirections function to check if we looped back to the beginning.
-- If we looped back to (0, 0) then we return Nothing
getDirectionsOut :: Maze -> Maybe [Direction]
getDirectionsOut m = if followDirections (0,0) (pathMaze (0, 0) North m) == (0,0)
    then Nothing
    else Just (pathMaze (0, 0) North m)

-- Question 5

data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
    deriving Show

ex1 = Unary
        (Unary
            (Unary
                (Unary
                    (Unary
                        (Unary
                            (Unary (Leaf 0) 1)
                        2)
                    3)
                4)
            5)
        6)
    7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex3 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))

ex5 :: Btree (Int, String)
ex5 = Binary (Binary (Leaf (0,"a")) (1,"z") (Leaf (2,"x"))) (3,"y") (Binary (Leaf (4,"b")) (5,"c") (Leaf (6,"d")))

-- Part i

-- A function that calculates the depth of a tree
depth :: Btree a -> Int
depth (Leaf x) = 1
depth (Unary t x) = depth t + 1
depth (Binary t x t2) = (depth t `max` depth t2) + 1

-- A function that checks if a tree is perfect by:
-- 1 - If the node is a Leaf then True is returned
-- 2 - If the node is a Unary then False is retuned (Making the function ultimately return False at the end because
-- if a Unary exists then its not a perfect tree)
-- 3 - If the node is Binary then we check if both depths are the same, check if left node is perfect and check if right node is perfect.
-- If any of the above is false then the function will ultimately return False at the end.
perfect :: Btree a -> Bool
perfect (Leaf x) = True
perfect (Unary t x) = False
perfect (Binary t x t2) = depth t == depth t2 && perfect t && perfect t2

-- Created a simple datatype to make things easier to handle
-- The first tuple value is for whether a tree/subtree is complete or not.
-- The second tuple stores the maximum depth of the tree/subtree
-- The third tuple stores the minimum deoth of the tree/subtree
type CompleteDepths = (Bool, Int, Int)

-- Gets first value (whether a tree is complete or not) from datatype CompleteDepths 
getCompleteValue :: CompleteDepths -> Bool
getCompleteValue (x, y, z) = x

checkComplete :: Btree a -> CompleteDepths
-- A leaf is considered complete so it returns True and has a max and min depth of 0
checkComplete (Leaf v) = (True, 0, 0)
-- A Unary is considered complete if its max depth is 0 (i.e. have one leaf node as child)
-- Otherwise recurse tree in the Unary
checkComplete (Unary l v) = (lMax == 0, 1 + lMax, 0) where
  (lComplete, lMax, lMin) = checkComplete l
-- A Binary is considered complete if both left and right nodes are complete (recursing through both),
-- left node min depth is greater or equal to the right node and the difference between
-- max depth of right and left node and the min depth of left and right node is no more than 1
-- The function also returns the max depth of the binary tree (the max of right and left node + 1)
-- and the min depth the binary tree (the min of right and left node + 1)
checkComplete (Binary l v r) =
  (lComplete && rComplete && lMin >= rMax && maxOfLR - minOfLR <= 1, 1 + maxOfLR, 1 + minOfLR) where
    (lComplete, lMax, lMin) = checkComplete l
    (rComplete, rMax, rMin) = checkComplete r
    maxOfLR = max lMax rMax
    minOfLR = min lMin rMin

-- Uses getCompleteValue and checkComplete to compute the final answer
complete :: Btree a -> Bool
complete x = getCompleteValue (checkComplete x)

-- Part ii

-- Check if the key passed in is the same as the key in Leaf, if its the same then we found the element
-- Otherwise return Nothing
lookupInSearchTree :: Int -> Btree (Int, a) -> Maybe a
lookupInSearchTree k (Leaf (x, v))
    | k == x = Just v
    | otherwise = Nothing

-- Check if the key passed in is the same as the key in Unary, if its the same then we found the element
-- If the key passed in is smaller then we call the function on the left node in Unary
-- Otherwise return Nothing
lookupInSearchTree k (Unary l (x, v))
    | k == x = Just v
    | k <= x = lookupInSearchTree k l
    | otherwise = Nothing

-- Check if the key passed in is the same as the key in Binary, if its the same then we found the element
-- If the key passed in is smaller then we call the function on the left Node
-- Otherwise we call the function on the right Node
lookupInSearchTree k (Binary l (x, v) r)
    | k == x = Just v
    | k <= x = lookupInSearchTree k l
    | otherwise = lookupInSearchTree k r

-- Part iii

insertInSearchTree :: Int -> a -> Btree (Int,a) -> Btree (Int,a)


-- If the node we want to insert our element to is a Leaf then convert it to a Unary with left node Leaf with our element as value.
insertInSearchTree x v (Leaf (lx, lv)) = Unary (Leaf (x, v)) (lx, lv)
-- If the node we want to insert our element to is a Unary then check if our key is bigger, if it is then
-- convert the Unary to Binary and insert our element to the right node of the Binary otherwise recurse to left child node of Unary with our element
insertInSearchTree x v (Unary l (ux, uv))
    | x <= ux = Unary (insertInSearchTree x v l) (ux, uv)
    | otherwise = Binary l (ux, uv) (Leaf (x, v))

-- If the node we want to insert our element to is a Binary then check if out key is bigger, if it is then
-- recurse to the right child node with our element  otherwise recurse to right child node with our element
insertInSearchTree x v (Binary l (bx, bv) r)
    | x <= bx = Binary (insertInSearchTree x v l) (bx, bv) r
    | otherwise = Binary l (bx, bv) (insertInSearchTree x v r)

