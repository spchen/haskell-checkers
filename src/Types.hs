module Types where 

import Prelude hiding ((!!))

import qualified Data.List
import qualified Data.Maybe as M 

import Debug.Trace
-------------------------------------------------------------------------------
--- Checkers Board ------------------------------------------------------------
-------------------------------------------------------------------------------

--R = Red--- B = Black --- R = Red King ---- BK = Black King
data Tile = EmptyTile | EmptyPlayTile | R | B | RK | BK
  deriving (Eq)

type Move   = ((Int,Int),(Int,Int))

type Board  = [[Tile]] 


(!!) :: Board -> (Int,Int) -> Tile
b!!(x,y) = (b Data.List.!! (x)) Data.List.!! (y)

-- Used to determine if a square is playable or not
whatSquare :: Int -> Int -> Tile
whatSquare x y = if (x `mod` 2 == y `mod` 2) then
                        EmptyPlayTile
                 else
                        EmptyTile

initalSquare :: Int -> Int -> Tile
initalSquare x y = if (x `mod` 2 == y `mod` 2) then
                        (if (y <= 3 && y >= 1) then
                          B
                        else
                          (if (y >=6 && y <= 8) then
                            R
                          else
                            EmptyPlayTile))
                 else
                        EmptyTile

emptyBoard :: Board
emptyBoard = [[whatSquare x y | y <-[1..8]] | x <- [1..8]]

startingBoard :: Board
startingBoard = [[initalSquare x y | y <-[1..8]] | x <- [1..8]]

-- to test winning scenarios (only puts two pieces on board)
testSquare x y =  if (x `mod` 2 == y `mod` 2) then
                    if x==4 && y==4 then B
                    else if x==5 && y==5 then R
                    else EmptyPlayTile
                  else EmptyTile

testBoard = [[testSquare x y | y <- [1..8]] | x <- [1..8]]

---Needs to check if there is a jump (player must do the jump) ----
validMoves :: Board -> Tile -> [Move]
validMoves b t = error "isneeded?"
--Maybe to check when there is a draw -- See scoreBoard in Checks.hs
--


---Jump Methods---
isJump :: Move -> Bool
isJump ((ox,oy),(nx,ny)) = abs (ox - nx) == 2 && abs (ny - oy) == 2

validList :: Board -> [Move] -> Tile -> Bool
validList b [] t = True
validList b (m@((ox,oy),(nx,ny)):ml) t
    | t == B = (ny - oy) == 2 && b!!(nx,ny) == EmptyPlayTile && b!!(removeWhich m) `elem` [R,RK] && validList b ml t
    | t == R = (oy - ny) == 2 && b!!(nx,ny) == EmptyPlayTile && b!!(removeWhich m) `elem` [B,BK] && validList b ml t
    | t `elem` [BK, RK] = abs (oy - ny) == 2 && b!!(nx,ny) == EmptyPlayTile && b!!(removeWhich m) `elem` [flipTile t, flipTile (unKingTile t)] && validList b ml t
    | otherwise = False

makeJump :: Board -> [Move] -> Tile -> Board
makeJump b [] t = b
makeJump b [m@((ox,oy),(nx,7))] B = makeSimpleMove (replaceTile b (removeWhich m) EmptyPlayTile) m BK
makeJump b [m@((ox,oy),(nx,0))] R = makeSimpleMove (replaceTile b (removeWhich m) EmptyPlayTile) m RK
makeJump b (m@((ox,oy),(nx,ny)):xs) t = makeJump (makeSimpleMove (replaceTile b (removeWhich m) EmptyPlayTile) m t) xs t

--todo write simpler
removeWhich :: Move -> (Int,Int)
removeWhich ((ox,oy), (nx,ny)) = let xi = if (nx > ox) then -1 else 1 in let xj = if (ny > oy) then -1 else 1 in (nx+xi, ny+xj)

--End Jump Methods---


putMaybe :: Board -> Tile -> [Move] -> Maybe Board
putMaybe b t (((ox,oy),(nx,ny)):ml) = 
  let m = ((ox,oy),(nx,ny)) in 
  case b!!(nx,ny) of
    EmptyPlayTile -> 
      if not $ isJump m then
        if isSimpleMove t m then
          case t of
            B ->  if (ny == 7 && b!!(ox,oy) /= BK) then
                    Just $ makeSimpleMove b m BK
                  else
                    Just $ makeSimpleMove b m B
            R ->  if (ny == 0 && b!!(ox,oy) /= RK) then
                    Just $ makeSimpleMove b m RK
                  else
                    Just $ makeSimpleMove b m R
        else
          case t of
            B -> Nothing
            R -> Nothing
            k -> Just $ makeSimpleMove b m k
      else
        -- tests for a valid jump
        if validList b (m:ml) t then
            Just $ makeJump b (m:ml) t
        else
            Nothing
    _         -> trace ("d"++(show (b!!(nx,ny)))) (trace ("s"++(show (b!!(ox,oy)))) Nothing)


makeSimpleMove :: Board -> Move-> Tile -> Board
makeSimpleMove b ((ox,oy),(nx,ny)) t = replaceTile (replaceTile b (ox,oy) EmptyPlayTile) (nx,ny) t

replaceTile :: Board -> (Int,Int) -> Tile -> Board
replaceTile b (ox,oy) t = 
    let new_col = replace (oy) t (b Data.List.!! (ox))
    in replace (ox) new_col b

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ x : drop (i+1) xs

-- No jumping required
-- Need to handle king case
isSimpleMove :: Tile -> Move -> Bool
isSimpleMove t ((ox,oy),(nx,ny)) = 
  case t of
    B -> (abs (nx - ox) == 1) && (ny - oy == 1)
    R -> (abs (nx - ox) == 1) && (ny - oy == -1)
    _ -> False

-------------------------------------------------------------------------------
--- Player --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = 
  Player { playerMove :: Tile -> Board -> IO [Move]
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
  show RK        = " RK  "
  show BK        = " BK  "

--showBoard :: Board -> String
--showBoard b = let blist = boardAsList b
--              in  unlines [Data.List.intercalate "|" row | row <- blist]
--              where
--                boardAsList b = [[show ((b Data.List.!! x) Data.List.!! y) | y <- [1..8]] | x <- [1..8]]
                  --[showRow b y | y <- [1..8]]

                 --[[show ((b Data.List.!! x) Data.List.!! y) | y <- [1..8]] | x <- [1..8]]

showBoard :: Board -> String
showBoard b = unlines [showRow b y | y <- [0..7]]

showRow :: Board -> Int -> String
showRow b i = showRowAux b i 7

showRowAux :: Board -> Int -> Int -> String
showRowAux b i 0 = show ((b Data.List.!! 0) Data.List.!! i)
showRowAux b i x = (showRowAux b i (x-1)) ++ "|" ++ (show ((b Data.List.!! x) Data.List.!! i) )
--not sure works plus not needed since implementing GUI
--showTileNumbers :: String
--showTileNumbers  = (unlines
--                   [Data.List.intercalate "|" ["(" ++ show x ++ "," ++ show y ++ ")" | y <- [1..8]] | x <- [1..8]])

flipTile :: Tile -> Tile
flipTile R = B 
flipTile B = R
flipTile BK = RK
flipTile RK = BK
flipTile EmptyTile = EmptyTile
flipTile EmptyPlayTile = EmptyPlayTile

kingTile :: Tile -> Tile
kingTile R = RK
kingTile B = BK
kingTile t = t

unKingTile :: Tile -> Tile
unKingTile RK = R
unKingTile BK = B
unKingTile _ = EmptyTile
