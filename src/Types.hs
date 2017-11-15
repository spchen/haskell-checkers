module Types where 

import Prelude hiding ((!!))

import qualified Data.List 
import qualified Data.Maybe as M 

-------------------------------------------------------------------------------
--- Checkers Board ------------------------------------------------------------
-------------------------------------------------------------------------------

--R = Red--- B = Black --- R = Red King ---- BK = Black King
data Tile = EmptyTile | EmptyPlayTile | R | B | RK | BK
  deriving (Eq)

type Move   = (Int,Int)

type Board  = [(Move, Tile)] 

(!!) :: Board -> Move -> Tile
b!!ij = M.fromMaybe EmptyTile (lookup ij b) 

whatSquare :: Int -> Int -> Tile
whatSquare x y = if (x `mod` 2 == y `mod` 2) then
                        EmptyPlayTile
                 else
                        EmptyTile

emptyBoard :: Board
emptyBoard = [((x,y), (whatSquare x y)) | x <- [1..8], y <- [1..8]]

startingBoard :: Board
startingBoard = [((x,y), (whatSquare x y)) | x <- [4..5], y <- [1..8]] ++ [((x,y), R) | x<-[1..3], y<-[1..8], (whatSquare x y) == EmptyPlayTile ] ++ 
    [((x,y), B) | x<-[6..8], y<-[1..8], (whatSquare x y) == EmptyPlayTile ] 


---Needs to check if there is a jump (player must do the jump) ----
validMoves :: Board -> Tile -> [Move]
validMoves b t = [(1,1)]

putMaybe :: Board -> Tile -> (Move,Move) -> Maybe Board
putMaybe b t (oldmove,newmove) = case b!!newmove of
-- TODO: place king tile if move is on opponents edge
               EmptyPlayTile -> Just $ map (\(m,ot) -> if m == oldmove then (m,EmptyPlayTile)
                                                    else if m == newmove then (m,t)
                                                    else (m,ot)) b 
               _         -> Nothing

-------------------------------------------------------------------------------
--- Player --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = 
  Player { playerMove :: Tile -> Board -> IO (Move,Move)
         , playerName :: String
         } 

-------------------------------------------------------------------------------
--- Score ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = [(PlayerInfo, Int)]

showFinalScore :: Score -> String
showFinalScore [(p1,i1),(p2,i2)]
  = if i1 == i2 
      then "Its a tie!" 
      else ("The winner is " ++ show (if i1 < i2 then p2 else p1))

showScore [(p1,i1),(p2,i2)] 
  = show p1 ++ " : " ++ show i1 ++ " VS. " ++ show p2 ++ " : " ++ show i2 
showScore _ 
  = ""

incr :: PlayerInfo -> Score -> Score
incr pi xs = map (\(pj,sj) -> if pi == pj then (pj,sj+1) else (pj,sj)) xs 

-------------------------------------------------------------------------------
--- Player Info ---------------------------------------------------------------
-------------------------------------------------------------------------------

data PlayerInfo =  
  PI { playerInfoPlayer :: Player
     , playerInfoTile   :: Tile
     , playerInfoInt    :: Int
     }

type Winner = PlayerInfo

instance Eq PlayerInfo where
    p1 == p2 = playerInfoInt p1 == playerInfoInt p2 

instance Show Player where
  show = playerName

instance Show PlayerInfo where
  show pi 
    | pname /= "Computer" && pname /= "Human"
    =  pname 
    | otherwise 
    = "Player " ++ show (playerInfoInt pi) 
    where pname = playerName $ playerInfoPlayer pi


instance Show Tile where
  show EmptyTile = "  .  " --- tiles that are Empty
  show EmptyPlayTile = "     " --- Tiles that are Empty that can be played
  show R         = "  R  "
  show B         = "  B  "
  show RK        = " RK "
  show BK        = " BK "

showBoard :: Board -> String
showBoard b = let blist = boardAsList b
              in  unlines [Data.List.intercalate "|" row | row <- blist]
              where
                boardAsList b = [[show (b!!(x,y)) | y <- [1..8]] | x <- [1..8]]

showTileNumbers :: String
showTileNumbers  = (unlines
                   [Data.List.intercalate "|" ["(" ++ show x ++ "," ++ show y ++ ")" | y <- [1..8]] | x <- [1..8]])
