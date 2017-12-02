module Players.Human (playerHuman) where 

import Types 
import Misc 
import System.IO
minus1 [] = []
minus1 (((ox,oy),(nx,ny)):ml) = ((ox-1,oy-1),(nx-1,ny-1)):(minus1 ml)

playerHuman :: String -> Player 
playerHuman = Player humanMove
  where
    humanMove _ _ = 
        do 
          ml <- (read <$> prompt "Write your move! [((src_col,src_row),(dest_col,dest_row))]\nwhere rows and cols are 1-8: ")
          return (minus1 ml)