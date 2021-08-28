{-
    Module: StoneWall.

-}
module Players.StoneWall where

import Types ( Game(Game), Player(..), Action(..), Board, Cell )
import Action 
import Game 
import Players.Dumb 
import Player 
import Board 
import Players.Minimax ()

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
stoneWallPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
stoneWallPlayerAction  b (p:ps) _ r 
    |validMoves == [] = Nothing
    |turn p == 1 && validStepAction (Game b (p:ps))( stepBottom ('e', 9))  =  Just(Move (stepBottom ('e', 9)))
    |turn p ==2 && validStepAction (Game b (p:ps))( stepBottom ('e', 8))  =  Just(Move (stepBottom ('e', 8)))
    |turn p ==3 && validWallAction (Game b (p:ps)) (wallTop ('d',8)) =  Just(Place  (wallTop ('d',8)))
    |turn p ==4 && validWallAction (Game b (p:ps)) (wallTop ('f',8)) =  Just(Place  (wallTop ('f',8)))
    |turn p ==5 && validWallAction (Game b (p:ps)) (wallTop ('b',8)) =  Just(Place  (wallTop ('b',8)))
    |turn p ==6 && validWallAction (Game b (p:ps)) (wallTop ('h',8)) =  Just(Place  (wallTop ('h',8)))
    |otherwise = Just (validMoves!!randomIdx)
    where 
        validMoves = [Move mv | mv<-allSteps, (canMove p ps mv) && (validStep b mv)]
        randomIdx = r `mod` (length validMoves)


--Stonewall with the minimax function. if my minimax works, uncomment and use this. not sure if it works though
-- could not test it. :(

    
{-
stoneWallPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
stoneWallPlayerAction  b (p:ps) _ r  
   |turn p == 1 && validStepAction (Game b (p:ps))( stepBottom ('e', 9))  =  Just(Move (stepBottom ('e', 9)))
    |turn p ==2 && validStepAction (Game b (p:ps))( stepBottom ('e', 8))  =  Just(Move (stepBottom ('e', 8)))
    |turn p ==3 && validWallAction (Game b (p:ps)) (wallTop ('d',8)) =  Just(Place  (wallTop ('d',8)))
    |turn p ==4 && validWallAction (Game b (p:ps)) (wallTop ('f',8)) =  Just(Place  (wallTop ('f',8)))
    |turn p ==5 && validWallAction (Game b (p:ps)) (wallTop ('b',8)) =  Just(Place  (wallTop ('b',8)))
    |turn p ==6 && validWallAction (Game b (p:ps)) (wallTop ('h',8)) =  Just(Place  (wallTop ('h',8)))
    |otherwise = stoneWallPlayerAction1  b (p:ps) _ r
stoneWallPlayerAction1 :: Board -> [Player] -> String -> Int -> Maybe Action
stoneWallPlayerAction1  b (p:ps) _ r  = let g = Game b (p:ps) in minimaxAction' g (minimax g)
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


-- We build a Stonewall player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makestoneWallPlayer :: String -> Cell -> Int -> [Cell] -> Player
makestoneWallPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = stoneWallPlayerAction } 
