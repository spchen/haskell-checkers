module Main where

import System.Exit

import Checks
import Types


main = 
    do 
        putStrLn "Running Check Tests"
        if noTileTest then
            putStrLn "[PASS] No Tile on Board"
         else
            do
                putStrLn "[FAIL] No Tile On Board"
                exitFailure 
        if tileWinsTest then
            putStrLn "[PASS] Tile Wins"
         else
            do
                putStrLn "[FAIL] Tile Wins"
                exitFailure
        if scoreBoardTest then
            putStrLn "[PASS] Score Board"
         else
            do
                putStrLn "[FAIL] Score Board"
                exitFailure
        exitSuccess
   

noTileTest :: Bool
noTileTest = (noTileOnBoard emptyBoard B)
    && (noTileOnBoard emptyBoard R)
    && (not $ noTileOnBoard startingBoard R)
    && (not $ noTileOnBoard startingBoard B)
    && (noTileOnBoard [[R | y<-[1..8]] | x <-[1..8]] B)
    && (not $ noTileOnBoard [[R | y<-[1..8]] | x <-[1..8]] R)
    && (noTileOnBoard [[B | y <-[1..8]] | x <-[1..8]] R)
    && (not $ noTileOnBoard [[B | y <-[1..8]] | x <-[1..8]] B)
    && (noTileOnBoard [[EmptyPlayTile | y <-[1..8]] | x<-[1..8]] B)

tileWinsTest :: Bool
tileWinsTest = (tileWins emptyBoard R)
    && (not $ tileWins startingBoard R)
    && (not $ tileWins startingBoard B)
    && (tileWins [[R | y<-[1..8]] | x <-[1..8]] R)
    && (tileWins [[B | y<-[1..8]] | x <-[1..8]] B)
    && (not $ tileWins [[R | y<-[1..8]] | x <-[1..8]] B)
    && (not $ tileWins [[B | y<-[1..8]] | x <-[1..8]] R)

scoreBoardTest :: Bool
scoreBoardTest = True
