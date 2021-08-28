{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board
import Player
import Game
import Players.Dumb (dumbAction)

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + maximum (map (stateTreeDepth . snd) ts)

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree g = StateTree g [(x,generateGameTree (fromJust (performAction g x))) | x <-validActions g]

{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}
{-High first seemed like a better option  to me as it was easier to implement the utility function. by ordering the tree the same
way for each player, i would have to implement the utility function according to perspective of the current player. since each player
would want to maximise their score, it only makes sense to order the tree according to high first.
-}
-- Higher scoring nodes go first.
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v a) = StateTree v (sorter [(c, highFirst d) | (c,d) <- a])
--function that maps the insertNode function to each edge in a list of edges
sorter :: (Ord v) => [(a,StateTree v b)] -> [(a,StateTree v b)]
sorter = foldr insertNode []
--function that given an edge and a list of edges, it inserts the edge into the list decreasingly
insertNode :: (Ord v) => (a,StateTree v b) -> [(a,StateTree v b)] -> [(a,StateTree v b)]
insertNode x [] = [x]
insertNode (a,StateTree v b) ((a2,StateTree v2 b2):ys)
    |v2 <= v = (a,StateTree v b):(a2,StateTree v2 b2):ys
    |otherwise = (a2,StateTree v2 b2) : insertNode (a,StateTree v b) ys


{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth x (StateTree v a )
    | x==0 =StateTree v []
    | otherwise = StateTree v [ (c,pruneDepth (x-1) d) | (c,d) <- a]



{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth x (StateTree v a ) = StateTree v (take x [ (c,pruneBreadth x d) | (c,d) <- a])

{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]
utility :: Game -> Int
utility (Game b p)
    | currentCell (currentPlayer p) `elem` winningPositions (currentPlayer p) = 999
    | otherwise = reachWin (reachableCells b (currentCell (currentPlayer p))) (winningPositions (currentPlayer p)) 999 b

reachWin ::[Cell] -> [Cell] -> Int -> Board -> Int
reachWin l1 l2  x b
    |  or[ a `elem`  l2 | a <- l1] = x
    | otherwise = reachWin (concat [reachableCells b c | c <- l1]) l2 (x-1) b




--reachWin (concatMap (reachableCells b ) l1) l2  (x-1) b
-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree
evalTree = mapStateTree utility

{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]


-- returns the last action added to the list of actions in a Result type
extractAction :: Result -> Action
extractAction (Result _ x) =  last (x)
minimaxFromTree :: EvalTree -> Action
minimaxFromTree et = extractAction(minimaxFromTree' [] et  True)
--if b is true than it is maximising. otherwise minimising. it recursively calls minimaxFromTree on the list of actions
-- until it reaches the leaf of the tree where it then returns a Result type with the value in the leaf and the list of
-- actions that  led to that leaf. it then compares the results and returns either the minimum or maximum result 
minimaxFromTree' :: [Action] -> EvalTree ->  Bool->Result
minimaxFromTree' x (StateTree i [] ) _ = Result i x
minimaxFromTree' x (StateTree i a )  b
    | b == True = maximum [minimaxFromTree' (c:x) d (not b) | (c , d) <- a]
    | otherwise  = minimum [minimaxFromTree' (c:x) d (not b) | (c , d) <- a]






{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]

minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree et = extractAction(maxAB [] (Result (-99999999) []) (Result (99999999) []) et (Result (-99999999) []) (Result (99999999) []))
 
--a,b
--if the player is maximising, it checks if the max of alpha and eval is greater than beta. if it is, it exits the loop. if it is
-- then it moves on to the next actions that was in the eval tree. eval is obtained by calling minAb on that action's evaltree.
--i have set the initial values of maxEval, minEval ,alpha and beta to very high and very low values (99999999 and -99999999).
-- this should be infinity and negative infinity but I did not know how to use infinity in haskell
maxAB :: [Action] -> Result -> Result -> EvalTree -> Result -> Result -> Result
maxAB a _ _ (StateTree i [] )  _ _ = Result i a
maxAB a alpha beta (StateTree i ((c,d):xs) )  maxEval minEval
    |(maxi alpha eval) >= beta = maxi maxEval eval
    |otherwise = maxAB a (maxi alpha eval) beta (StateTree i (xs) ) (maxi eval maxEval) minEval
    where eval = minAB (c:a) alpha beta d  maxEval (Result (99999999) [])

minAB :: [Action] -> Result -> Result -> EvalTree -> Result -> Result -> Result
minAB a _ _ (StateTree i [] )  _ _= Result i a
minAB a alpha beta (StateTree i ((c,d):xs) )  maxEval minEval
    |alpha >= (mini beta eval) = mini minEval eval
    |otherwise =minAB a alpha (mini eval beta) (StateTree i (xs) ) maxEval (mini minEval eval)
    where eval = maxAB (c:a) alpha beta d  (Result (-99999999) []) minEval

-- given 2 result types the function returns the result with the higher score
maxi :: Result -> Result -> Result
maxi (Result i1 act1 ) (Result i2 act2)
    | i1 >= i2 = (Result i1 act1 )
    | otherwise = (Result i2 act2)
-- given 2 result types the function returns the result with the lower score
mini :: Result -> Result -> Result
mini (Result i1 act1 ) (Result i2 act2)
    | i1 <= i2 = (Result i1 act1 )
    | otherwise = (Result i2 act2)


{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int
depth = 4

-- Given breadth for pruning.
breadth :: Int
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c,
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
