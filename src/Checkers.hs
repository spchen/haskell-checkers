module Main where 

import Prelude hiding ((!!))

import Types 
import Checks 
import Misc 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Players.Human    (playerHuman   )

import Debug.Trace




player1, player2 :: Player
player1 = playerHuman "Player1"
player2 = playerHuman "Player2"

--GLOSS STUFF---------------------------
window :: Display
window = InWindow "NCheckers" (600, 600) (10, 10)

renderBoard :: Board -> Picture
renderBoard b
  | (tileWins b R && tileWins b RK) || (tileWins b B && tileWins b BK)
    = pictures [boardbg, (pictures playTiles), (renderPieces b), gameOver]
  | otherwise
    = pictures [boardbg, (pictures playTiles), (renderPieces b)]
  where
    boardbg = color (dark white) $ rectangleSolid 480 480
    playTiles = [translate (fromIntegral (2*x -9)*30) (fromIntegral (9-2*y)*30) $ color (greyN 0.6) $ rectangleSolid 60 60 | x <- [1..8], y <- [1..8], (x-1) `mod` 2 == (y-1) `mod` 2]
    gameOver = translate (-100) 250 $ scale 0.3 0.3 $ Text "Game Over"

renderPieces :: Board -> Picture
renderPieces b = pictures [(pictures pieces), (pictures kings)]
    where
        pieces = [translate (fromIntegral (2*x-9)*30) (fromIntegral (9-2*y)*30) $ color pc $ circleSolid 25 | x <- [1..8], y <- [1..8], pc <- [black, dark red], t<-[b!!(x-1,y-1)], t == R && pc == dark red || t == B && pc == black]
        -- pretty sure no background
        kings = [translate (fromIntegral (2*x-9)*30 -25) (fromIntegral (9-2*y)*30 -15) $ color white $ scale 0.3 0.3 $ Text kstr | x <- [1..8], y <-[1..8], t<-[b!!(x-1,y-1)], kstr <- ["BK","RK"], t == BK && kstr == "BK" || t == RK && kstr == "RK"]
 
type State = (Board, (Int,Int), Tile)

handleEvent :: Event -> State -> State
-- | Left mouse click
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state@(b,(_,_),t)
    | x' < 0 || x' > 7 || y' < 0 || y' > 7 = state-- If pressed outside board do nothing
    | otherwise =  trace ("handleEvent MouseButton Down:"++(show ((x',y'),t))) (b,(x',y'),t) -- update state
    where 
      x' = (round $ (x+270)/60) - 1 
      y' = (round $ (270-y)/60) - 1
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) state@(b,(ox,oy),t)
    | isJump m && trace (show (length (validJumps nx ny b pieceToMove))) (length (validJumps nx ny b pieceToMove)) > 0 && pieceToMove `elem` fullTileSet
      = case (putMaybe b pieceToMove [m]) of
          Just nb -> (nb,(nx,ny),t)
          Nothing -> state
    | pieceToMove `elem` fullTileSet
      = case (trace ("handleEvent MouseButton Up:"++(show m)) (putMaybe b pieceToMove [m])) of
          Just nb -> (nb,(0,0),flipTile t)
          Nothing -> state
    | otherwise = 
      let msg = "Must move "++(show t)++" tile on this turn: "++(show ((ox,oy),t))
        in trace msg state
    where 
      pieceToMove = b!!(ox,oy)
      m = ((ox,oy),(nx,ny))
      nx = (round $ (x+270)/60) - 1
      ny = (round $ (270-y)/60) - 1
      fullTileSet = [t,kingTile t]
handleEvent _ state = state -- Rest of the events - no response

validJumps :: Int -> Int -> Board -> Tile -> [(Int,Int)]
validJumps x y b B = [(x+jx,y+2) | jx <- [-2,2], x+jx<=7, x+jx>=0, y+2<=7, y+2>=0, b!!(x+jx,y+2) == EmptyPlayTile, b!!(removeWhich ((x,y),(x+jx,y+2))) `elem` [R,RK]]
validJumps x y b R = [(x+jx,y-2) | jx <- [-2,2], x+jx<=7, x+jx>=0, y-2<=7, y-2>=0, b!!(x+jx,y-2) == EmptyPlayTile, b!!(removeWhich ((x,y),(x+jx,y-2))) `elem` [B,BK]]
validJumps x y b BK = [(x+jx,y+jy) | jx <- [-2,2], jy <- [-2,2], x+jx<=7, x+jx>=0, y+jy<=7, y+jy>=0, b!!(x+jx,y+jy) == EmptyPlayTile, b!!(removeWhich ((x,y),(x+jx,y+jy))) `elem` [R,RK]]
validJumps x y b RK = [(x+jx,y+jy) | jx <- [-2,2], jy <- [-2,2], x+jx<=7, x+jx>=0, y+jy<=7, y+jy>=0, b!!(x+jx,y+jy) == EmptyPlayTile, b!!(removeWhich ((x,y),(x+jx,y+jy))) `elem` [B,BK]]
--END GLOSS STUFF-----------------------

main :: IO ()
main = do
    gui

gui = do
      play window white 0 state render handleEvent (\_ y-> y)
      where
        state = (startingBoard, (0,0), B)
        render = \(b,(x,y),t) -> renderBoard b

ascii = do
        putStrLn "Let's play Checkers!."
        rounds  <- prompt "How many rounds should we play?"
        score   <- playRounds (read rounds) player1 player2 
        putStrLn $ showFinalScore score 


playRounds :: Int -> Player -> Player -> IO Score
playRounds rounds player1 player2 = 
  foldM (playRound pi1 pi2) [(pi1,0),(pi2,0)] [1..rounds]
  where 
    pi1 = PI player1 B 1 
    pi2 = PI player2 R 2 

playRound :: PlayerInfo -> PlayerInfo -> Score -> Int -> IO Score 
playRound p1 p2 score i = do 
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if (i `mod` 2 == 0) then show p2 else show p1)  ++ " plays first")
   putStrLn (showBoard startingBoard)
   result <- if (i `mod` 2 == 0) then mplay p2 p1 startingBoard else mplay p1 p2 startingBoard
   case result of 
      Just p  -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Nothing -> putStrLn "Its a draw!\n\n" >> return score 


mplay :: PlayerInfo -> PlayerInfo -> Board -> IO (Maybe Winner)
mplay pi1@(PI p1 t1 _) pi2 board = do
  move <- (playerMove p1) t1 board
  case putMaybe board t1 move of
    Nothing -> putStrLn "Invalid move." >> return (Just pi2)
    Just b  -> do putStrLn $ showBoard b
                  if tileWins b t1
                    then return (Just pi1) 
                    else mplay pi2 pi1 b 
