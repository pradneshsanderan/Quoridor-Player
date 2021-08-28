{-
    Module: Main.

    This is the module that needs to be compiled in order to play the game. It implements the
    interactive components.
-}
module Main where 

import Data.Char
import Data.String
import Data.Graph
import Data.Maybe
import Control.Monad
import Control.Monad.Random

import Types
import Constants
import Cell
import Player
import Players.Human 
import Players.Dumb 
import Players.Minimax
import Players.Reed
import Players.StoneWall
import Game 
import Print

{-
    Some defaults to be able to play.
-}

-- All the cells.
startingCells :: [Cell]
startingCells = [(i, j) | i<-allColumns, j<-allRows]

-- All the edges.
startingEdges :: [(Cell, Cell, [Cell])]
startingEdges = [(c, c, adjacent c) | c<-startingCells]
    where 
        adjacent :: Cell -> [Cell]
        adjacent c = [c' | c'<-startingCells, isAdjacent c c']

-- A board (graph) formed from all the edges.
startingBoard :: Board 
startingBoard = b 
    where 
        (b, _, _) = graphFromEdges startingEdges

-- Takes two player 'constructors' (e.g. makeHumanPlayer, makeMinimaxPlayer, ...) and returns a list
-- of two players named "X" and "Y" that start from the middle column in the top and the bottom row
-- respectively.
startingPlayersMiddle :: (String -> Cell -> Int -> [Cell] -> Player) -> 
                        (String -> Cell -> Int -> [Cell] -> Player) -> 
                        (String -> Cell -> Int -> [Cell] -> Player) -> 
                         (String -> Cell -> Int -> [Cell] -> Player) -> [Player]
startingPlayersMiddle ctr1 ctr2 ctr3 ctr4 = 
    [ctr1 "P1" (middleColumn, firstRow) wallsPerPlayer winningX, 
     ctr2 "P2" (middleColumn, lastRow) wallsPerPlayer winningY,
     ctr2 "P3" (lastColumn , middleRow) wallsPerPlayer winningP3,
     ctr2 "P4" (firstColumn , middleRow) wallsPerPlayer winningP4]
    where 
        middleRow = (boardSize `div` 2)+1
        middleColumn = intToColumn ((div boardSize 2) + 1)
        winningX = [(i, lastRow) | i<-allColumns]
        winningY = [(i, firstRow) | i<-allColumns]
        winningP3 = [(firstColumn , j) | j<-allRows ]
        winningP4 = [(lastColumn , j) | j<-allRows ]

-- Translates a string to the corresponding player constructor, useful for initialisation.
nameToPlayerConstructor :: String -> Maybe (String -> Cell -> Int -> [Cell] -> Player)
nameToPlayerConstructor "Human" = Just makeHumanPlayer
nameToPlayerConstructor "Dumb" = Just makeDumbPlayer
nameToPlayerConstructor "Minimax" = Just makeMinimaxPlayer
nameToPlayerConstructor "Reed" = Just makeReedPlayer
nameToPlayerConstructor "Stonewall" = Just makeStoneWallPlayer
nameToPlayerConstructor _ = Nothing

{-
    Interactive part of the code. This is what makes the game playable.
-}

-- Game loop. It proceeds in several steps:
-- 1. Checks if the game is over. If not, continues.
-- 2. Prints the state of the game.
-- 3. Reads the command and processes it, updating the game (and the turn).
-- 4. Back to 1.
play :: Game -> IO ()
play g@(Game b ps) = 
    let p1 = currentPlayer ps 
        p2 = previousPlayer ps in 
    if (hasWon p2) 
        then do { putStrLn ("\nPlayer " ++ (name p2) ++ " wins."); printGame g }
       else do {
            putStrLn ("\nWould you like to forfeit the game? Y/N");
            forfeit<-getLine;
            if (forfeit == "Y" || forfeit == "y" ) then do  {printGame g; putStrLn ("\nPlayer " ++ (name p1) ++ " forfeited.");}
            else do{
            putStrLn ("\nPlayer " ++ (name p1) ++ "'s turn."); printGame g; 
            r <- evalRandIO rand;
            if (isHuman p1) 
                then do { command<-getLine; playCommand p1 b ps command r }
                else do { playCommand p1 b ps "" r } }}
    where 
        -- Calls chooseAction (from current player) and if the action is valid, calls performAction.
        playCommand :: Player -> Board -> [Player] -> String -> Int -> IO ()
        playCommand p b ps command r = 
            case (chooseAction p b ps command r) of 
                Nothing -> do { putStrLn "Invalid input."; play g } 
                Just a ->
                    case (performAction g a) of 
                        Nothing -> do { putStrLn "Invalid action."; play g }
                        Just g' -> do { play g' }
        
        -- Simple random number generator (can be used by the player).
        rand :: (RandomGen g) => Rand g Int
        rand = getRandomR (0, 10000)

-- This is the function that you should run to play the game. It asks about the type of game you 
-- want to play and then calls the main game loop.
main :: IO ()
main = do {
    putStrLn "What kind of player is player X? (Human/Dumb/Minimax/Reed)";
    playerX<-getLine;
    putStrLn "What kind of player is player Y? (Human/Dumb/Minimax/Reed/StoneWall)";
    playerY<-getLine; 
    putStrLn "What kind of player is player P3? (Human/Dumb/Minimax/Reed)";
    playerP3<-getLine; 
    putStrLn "What kind of player is player P4? (Human/Dumb/Minimax/Reed)";
    playerP4<-getLine; 
    case (nameToPlayerConstructor playerX, nameToPlayerConstructor playerY, nameToPlayerConstructor playerP3, nameToPlayerConstructor playerP4) of 
        (Just ctrX, Just ctrY,Just ctrP3,Just ctrP4) -> do { play (Game startingBoard (startingPlayersMiddle ctrX ctrY ctrP3 ctrP4)) }
        _ -> do { putStrLn "Unrecognised player type. Try again."; main } }
