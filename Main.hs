{--------------------------------------------------------------------------------
  Copyright (c) 2003 Daan Leijen.
  "Time flows like a river" -- an old Fran demo :-)
  Demonstrates the use of an idle event handler to implement a resource
  aware gui that still does heavy animation.
--------------------------------------------------------------------------------}
module Main where

import System.CPUTime
import Graphics.UI.WXCore (getTextExtent)
import Graphics.UI.WX
import WxFRP
import FRPLib
import Control.Monad.IO.Class

{-------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------}
-- Time is in seconds, represented as a double.
type Time = Double
type Duration = Double
-- The (mouse) history consists of time/position pairs (and is never null)
type History a = [(Time,a)]


           
remember :: Duration -> Step (Time,a) -> Behavior (Step (History a))
remember dur s = 
  do now <- time
     init <- snd <$> sample s
     scan addPrune [(now,init)] (updates s)
 where addPrune l (t,v) = (t,v) : takeWhile before l where
        cutoffTime = t - dur
        before (tv,_) = tv >= cutoffTime

getTimeStep :: Events x -> Behavior (Step Time)
getTimeStep e = do t <- time
                   step t (time <@ e)

{-------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------}
-- The total time lag of the words behind the mouse cursor
timeSpan :: Time
timeSpan = 10

-- The flowing text
flowText = "Time flows like a river, bananas crumble like a donkey"

-- The font style
flowFont = fontSwiss{ _fontSize = 16 }


{-------------------------------------------------------------------------
  The gui
-------------------------------------------------------------------------}
main
  = runWXFrp timeFlows


timeFlows = 
 do p <- liftIO $ 
     do 

        -- create a frame.
        f <- frame   [ text        := flowText]      
        p <- panel f []                          -- draw in a panel



        -- set layout
        set f        [ layout      := fill $ widget p
                     , clientSize  := sz 300 300       -- initial size
                     ]
        return p
    mouseHist <- liftIO $ varCreate [(0,pt 0 0)]
    liftIO $        -- set event handlers
     do -- mouse history as list of time/position pairs: is never null!
        --vmouseHistory <- 
        set p        [ on paint    := onPaint  mouseHist 
                     --, on idle     := onIdle   vmouseHistory p
                     -- , on motion   := onDrag   vmouseHistory
                     ]

    mouseMoves <- setSource motion p
    s <- scan (\x _ -> x + 1) 0 mouseMoves
    mousePos <- step (pt 0 0) mouseMoves
    idle <- getIdle p
    times <- b $ getTimeStep idle
    mouseHis <- b $ remember timeSpan ((,) <$> times <*> mousePos)
    plan (doIt mouseHist p <$> mouseHis <@ idle) 
    plan (liftIO . putStrLn . show <$> updates s)
    --plan (showTime <$> e)
    return ()

{-------------------------------------------------------------------------
  Event handlers
-------------------------------------------------------------------------}
-- repaint handler
onPaint vmouseHistory  dc viewArea
  = do history <- varGet vmouseHistory
       time    <- getTime
       -- draw trace line
       polyline dc (map snd history) [penColor := lightgrey]
       -- draw the words
       set dc [font := flowFont ]
       mapM_ drawWord (wordPositions history timeSpan time flowText)
  where
    drawWord (pos,word)
      = do -- center word
           sz <- getTextExtent dc word
           let newX = pointX pos - (sizeW sz `div` 2)
               newY = pointY pos - (sizeH sz `div` 2)
           -- and draw it.
           drawText dc word (pt  newX newY) []

        
-- idle event handler
doIt :: Var (History Point) -> Window a -> History Point -> Now ()
doIt vmouseHistory win history
  = liftIO $
    do varSet vmouseHistory history
       if (null (tail history))
        then return ()
        else do repaint win
                -- prune the history  
           
-- idle event handler
onIdle :: Var (History Point) -> Window a -> IO Bool
onIdle vmouseHistory win
  = do history <- varGet vmouseHistory
       if (null (tail history))
        then do -- don't call idle again until some other event happens
                return False
        else do time <- getTime
                repaint win
                -- prune the history  
                varSet vmouseHistory (prune time history)
                return True
	where
	    -- prune the history: only remember time/position pairs up to a certain time span.
	    prune time (h:hs)
	      = h:takeWhile (after (time-timeSpan)) hs

	    after time (t,p)
	      = time <= t




-- mouse drag handler
onDrag vmouseHistory mousePos
  = do time <- getTime
       -- prepend a new time/position pair
       varUpdate vmouseHistory ((time,mousePos):)
       return ()
           

{-------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------}
-- Tuple each word in a string with its historic position, given a mouse
-- history, a time span, and current time.
wordPositions :: History Point -> Time -> Time -> String -> [(Point,String)]
wordPositions history timeSpan time 
  = wordPositionsAt history . wordTimes timeSpan time . words 

-- Translate time/word pairs to position/word pairs given the mouse position history.
wordPositionsAt :: History Point -> [(Time,String)] -> [(Point,String)]
wordPositionsAt history timedWords
  = [(posAtTime t history, word) | (t,word) <- timedWords]

lerp :: Double -> Double -> Double -> Double
lerp t a b = (1 - t) * a  + t * b

lerpPoint :: Double -> Point -> Point -> Point
lerpPoint t p q = pt (round $ lerp t' (fromIntegral $ pointX p) (fromIntegral $  pointX q)) (round $ lerp t' (fromIntegral $ pointY p) (fromIntegral $ pointY q)) 
      where t' = max 0 (min 1 (t))
-- | Return the mouse position at a certain time.
posAtTime :: Time -> History Point -> Point
posAtTime time [(t,pos)]    = pos
posAtTime time ((ta,pa): (tb,pb) : t) 
      | time >= tb = lerpPoint ((time - tb) / (ta - tb)) pb pa
      | otherwise = posAtTime time ((tb,pb) : t)

-- | Evenly assign times to the words in a string, given a timeSpan and current time.
wordTimes :: Time -> Time -> [String] -> [(Time,String)]
wordTimes timeSpan time words
  = let n     = length words
        delta = timeSpan / (fromIntegral n)
    in zip (iterate (\t -> t-delta) time) words
    
-- Get the current Time
getTime :: IO Time
getTime
  = do picoSecs <- getCPUTime
       let time = (fromIntegral picoSecs) / 1.0e12
       return time
