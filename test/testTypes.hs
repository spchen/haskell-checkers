module Main where

import System.Exit

import Types


main = 
    do 
        putStrLn "Running Types Tests"
        if whatSquareTest then
            putStrLn "[PASS] WhatSquareTest"
         else
            do
                putStrLn "[FAIL] WhatSquareTest"
                exitFailure 
        if initalSquareTest then
            putStrLn "[PASS] InitalSquareTest"
         else
            do
                putStrLn "[FAIL] initalSquareTest"
                exitFailure
        if isJumpTest then
            putStrLn "[PASS] isJump"
         else
            do
                putStrLn "[FAIL] isJump"
                exitFailure
        if isSimpleMoveTest then
            putStrLn "[PASS] isSimpleMove"
         else
            do
                putStrLn "[FAIL] isSimpleMove"
                exitFailure 
        exitSuccess
   

whatSquareTest :: Bool
whatSquareTest = whatSquare 0 0 == EmptyPlayTile

initalSquareTest :: Bool
initalSquareTest = [[initalSquare x y | y <-[1..8]] | x <-[1..8]] == [ [B, EmptyTile, B, EmptyTile, EmptyPlayTile, EmptyTile, R, EmptyTile], [EmptyTile, B, EmptyTile, EmptyPlayTile, EmptyTile, R, EmptyTile, R], [B, EmptyTile, B, EmptyTile, EmptyPlayTile, EmptyTile, R, EmptyTile], [EmptyTile, B, EmptyTile, EmptyPlayTile,  EmptyTile, R, EmptyTile, R], [B, EmptyTile, B, EmptyTile, EmptyPlayTile, EmptyTile, R, EmptyTile], [EmptyTile, B, EmptyTile, EmptyPlayTile,  EmptyTile, R, EmptyTile, R], [B, EmptyTile, B, EmptyTile, EmptyPlayTile, EmptyTile, R, EmptyTile], [EmptyTile, B, EmptyTile, EmptyPlayTile,  EmptyTile, R, EmptyTile, R]]

isJumpTest :: Bool
isJumpTest = (isJump ((1,1),(3,3)))
    && (not $ isJump ((1,1),(1,1)))
    && (not $ isJump ((1,1),(2,1)))
    && (isJump ((3,3),(1,1)))
    && (not $ isJump ((2,1),(2,0)))

isSimpleMoveTest :: Bool
isSimpleMoveTest =
    (not $ isSimpleMove B ((0,0),(8,8)) )
    && (isSimpleMove B ((0,0),(1,1)))
    && (not $ isSimpleMove R ((0,0),(1,1)))
    && (not $ isSimpleMove B ((1,1),(0,0)))
    && (isSimpleMove R ((1,1),(0,0)))
