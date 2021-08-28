{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import Types
import Action
import Game
import Players.Dumb 
import Player (canMove)
import Board (validStep)
import Players.Minimax

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction  b (p:ps) _ r 
    |validMoves == [] = Nothing
    |turn p == 1 && validWallAction (Game b (p:ps)) (wallTop ('c',3)) =  Just(Place  (wallTop ('c',3)))
    |turn p ==2 && validWallAction (Game b (p:ps)) (wallTop ('f',3)) =  Just(Place  (wallTop ('f',3)))
    |otherwise = Just (validMoves!!randomIdx)
    where 
        validMoves = [Move mv | mv<-allSteps, (canMove p ps mv) && (validStep b mv)]
        randomIdx = r `mod` (length validMoves)


--Reed with the minimax function. if my minimax works, uncomment and use this. not sure if it works though
-- could not test it. :(

    
{-
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction  b (p:ps) _ r  
    |turn p == 1 && validWallAction (Game b (p:ps)) (wallTop ('c',3)) =  Just(Place  (wallTop ('c',3)))
    |turn p ==2 && validWallAction (Game b (p:ps)) (wallTop ('f',3)) =  Just(Place  (wallTop ('f',3)))
    |otherwise = reedPlayerAction1  b (p:ps) _ r
reedPlayerAction1 :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction1  b (p:ps) _ r  = let g = Game b (p:ps) in minimaxAction' g (minimax g)
    where
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."
-}


-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedPlayerAction } 
