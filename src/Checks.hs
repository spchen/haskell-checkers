module Checks where

import Prelude hiding ((!!))

import Types 

flipTile :: Tile -> Tile
flipTile R = B 
flipTile B = R
flipTile Bk = Rk
flipTile Rk = Bk
flipTile _ = EmptyTile

kingTile :: Tile -> Tile
kingTile R = RK
kingTile B = BK
kingTile _ = EmptyTile

p1wins, p2wins :: Board -> Bool
p1wins b = tileWins b R
p2wins b = tileWins b B

-- All opponents checkers eliminated
-- OR
-- TODO: opponent has no valid moves
tileWins :: Board -> Tile -> Bool
-- eliminates all opponents pieces or forces the opponent to have no moves
tileWins b t = 
   noTileOnBoard b (flipTile t) -- || (validMoves b (flipTile t)) == []

noTileOnBoard:: Board -> Tile -> Bool
noTileOnBoard b t = not (any (\row-> any (\col -> b!!(row,col) == t) [1..8]) [1..8])

scoreBoard :: Tile -> Board -> Maybe Int 
scoreBoard tile board 
  | tileWins board tile 
  = Just 1
  | tileWins board (flipTile tile)   
  = Just (-1) 
-- neither player has any valid moves = draw
-- | (validMoves board tile) == [] && (validMoves board (flipTile tile)) == []           
 -- = Just 0
  | otherwise
  = Nothing 
