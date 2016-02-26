{-# Language OverloadedStrings,RecursiveDo #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GlossFRP
import FRPLib
import AllReal
import Control.Monad.IO.Class()

main :: IO ()
main = glossFRP (InWindow "FRP!" (1024,800) (0,0))  white 60 mainFRP


mainFRP :: Events Event -> Events Float -> Now (Behavior Picture)
mainFRP evs deltaTime =
  do time <- scan (+) 0 deltaTime

     let mouseMoves = filterJust $ getMouseMove <$> evs
     mousePos <- step (-1000,-1000) mouseMoves

     let makeBall p = ballb p evs mousePos time

     let rightClicks = filtere (isClick RightButton) evs
     let clickedAt = mousePos <@ rightClicks

     let newBalls = observeE (makeBall <$> clickedAt)
     balls <- dynList newBalls

     return $ Pictures <$> balls

getMouseMove :: Event -> Maybe Point
getMouseMove (EventMotion p) = Just p
getMouseMove _               = Nothing

getClick :: Event -> Maybe MouseButton
getClick (EventKey (MouseButton m) Up _ _) = Just m
getClick _                                = Nothing

isClick :: MouseButton -> Event -> Bool
isClick x (EventKey (MouseButton b) Down _ _) | x == b = True
isClick _ _                                = False

isRelease :: MouseButton -> Event -> Bool
isRelease b (EventKey (MouseButton x) Up _ _) | x == b = True
isRelease _ _                                = False




type Time = Float
-- type Point = (Float,Float)

ballb :: Point -> Events Event -> Behavior Point -> Behavior Float -> Now (Behavior Picture)
ballb initialPos evs mousePos time = mdo
        curTime <- sample time
        let localTime = (\x -> x - curTime) <$> time
        let radius = radiusAtTime <$> localTime
        let cirkel = Cirkel <$> pos <*> radius
        let mouseOver = isInside <$> mousePos <*>. cirkel
        let startDrag = leftClicks evs `during` mouseOver
        let draggingUpdates = (True <$ startDrag) `union` (False <$ leftReleases evs)
        isDragging <- hold False draggingUpdates
        let color = chooseColor <$> isDragging
        let entersBall = when mouseOver
        nrEnters <- scan (\x _ -> x + 1) 0 entersBall
        let moused = deltas mousePos `during` isDragging
        pos <- addDeltas initialPos moused
        return $ ball <$> color <*> cirkel

radiusAtTime t = 35 + 25 * sin t

chooseColor True = green
chooseColor False = red
leftClicks evs = filtere (isClick LeftButton) evs
leftReleases evs = filtere (isRelease LeftButton) evs

deltas :: Behavior Point -> Events Point
deltas b = (.-) <$> updates b >@>> prev b

addDeltas :: Point -> Events Point -> Now (Behavior Point)
addDeltas i e = scan (.+) i e

data Cirkel = Cirkel { center :: Point, radius :: Float }

lerpColor :: Color ->  Color -> Float -> Color
lerpColor c  d f = makeColor (lerp r f r') (lerp g f g') (lerp b f b') (lerp a f a')
  where
        (r,g,b,a) = rgbaOfColor c
        (r',g',b',a') = rgbaOfColor d

lerp :: Float -> Float -> Float -> Float
lerp a i b = (1 - i') * a + i' * b
  where i' = max 0 (min 1 i)

isInside ::  Point -> Cirkel -> Bool
isInside  (x',y') (Cirkel (x,y) r) =
  let xdiff = x - x'
      ydiff = y - y'
  in xdiff * xdiff + ydiff * ydiff <= r * r


ball :: Color -> Cirkel ->  Picture
ball c (Cirkel (x,y) r) = Translate x y $ Color c $ circleSolid r





-- type Point = (Float,Float)




type Vec = (Float,Float)
(.+) :: Vec -> Vec -> Vec
(x,y) .+ (x',y') = (x + x', y + y')

(.*.) :: Vec -> Vec -> Vec
(x,y) .*. (x',y') = (x * x', y * y')

(.-) :: Vec -> Vec -> Vec
(x,y) .- (x',y') = (x - x', y - y')

(.*) :: Float -> Vec -> Vec
t .* (x,y) = (t * x, t * y)

(./) :: Vec -> Float -> Vec
(x,y) ./ t = (x / t, y / t)


{-
example :: Events Event -> Behavior Point -> Behavior Time -> Now (Behavior Picture)
example evs mousePos time =
  do let rightClicks = filtere (isClick RightButton) evs
     let ballStarts  = mousePos `at` rightClicks
     let newBall p = ballb p evs mousePos time
     let newBalls = observeE $ newBall <$> ballStarts
     balls <- dynList newBalls
     return (Pictures <$> balls)


ballb :: Point -> Events Event -> Behavior Point -> Behavior Time -> Now (Behavior Picture)
ballb c evs mousePos time = mdo
     let leftClicks  = filtere (isClick LeftButton) evs
     let leftRelease = filtere (isRelease LeftButton) evs
     let dragStart   = leftClicks `during` mouseOver
     let dragEnd     = leftRelease

     isDragging <- hold False ((True <$ dragStart) `union` (False <$ dragEnd))

     let mouseMoves = derivative mousePos `during` isDragging
     pos <- integrate c mouseMoves

     let mouseOver = isInside <$> mousePos <*> cirkel
     let cirkel = Cirkel <$> pos <*> (mkRadius <$> time)
     let farg = chooseColor <$> mouseOver


     pure $ ball <$> farg <*> cirkel where

  chooseColor True  = green
  chooseColor False = red

  mkRadius t = 35 + 25 * sin t




derivative :: Behavior Point -> Events Point
derivative s = (.-) <$> updates s >@> prev s

integrate :: Point -> Events Point -> Now (Behavior Point)
integrate = scan (.+)
-}

{-

example :: Events MouseButton -> Behavior Point -> Behavior Time -> Behavior Picture
example clicks mousePos time = ball <$> color <*> circle where
  color = chooseColor <$> mouseOver
  chooseColor True = green
  chooseColor False = red
  mouseOver = isInside <$> mousePos <*> circle
  circle = mkCircle <$> time
  mkCircle t = Cirkel (0,0) (35 + 25 * sin t)


data Cirkel = Cirkel { center :: Point, radius :: Float }


isInside ::  Point -> Cirkel -> Bool
isInside  (x',y') (Cirkel (x,y) r) =
  let xdiff = x - x'
      ydiff = y - y'
  in xdiff * xdiff + ydiff * ydiff <= r * r


ball :: Color -> Cirkel ->  Picture
ball c (Cirkel (x,y) r) = Translate x y $ Color c $ circleSolid r

-}



{-
mainFRP :: Events Event -> Events Float -> Now (Behavior Picture)
mainFRP evs deltaTime =
  do time <- scan (+) 0 deltaTime

     let mouseMoves = filterJust $ getMouseMove <$> evs
     mousePos <- step (-1000,-1000) mouseMoves

     let mouseClicks = filterJust $ getClick <$> evs

     example mouseClicks mousePos time

getMouseMove (EventMotion p) = Just p
getMouseMove _               = Nothing

getClick (EventKey (MouseButton m) Up _ _) = Just m
getClick _                                = Nothing



type Time = Float
-- type Point = (Float,Float)

example :: Events MouseButton -> Behavior Point -> Behavior Time -> Now (Behavior Picture)
example clicks mousePos time =
  do let leftClicks = filtere ((==) LeftButton) clicks
     let clicksOn   = leftClicks `during` mouseOver
     active <- scan (\x _ -> not x) False clicksOn
     pure $ ball <$> (chooseColor <$> active) <*> circle where
  color = chooseColor <$> mouseOver
  chooseColor True = green
  chooseColor False = red
  mouseOver = isInside <$> mousePos <*> circle
  circle = mkCircle <$> time
  mkCircle t = Cirkel (0,0) (35 + 25 * sin t)


data Cirkel = Cirkel { center :: Point, radius :: Float }


isInside ::  Point -> Cirkel -> Bool
isInside  (x',y') (Cirkel (x,y) r) =
  let xdiff = x - x'
      ydiff = y - y'
  in xdiff * xdiff + ydiff * ydiff <= r * r


ball :: Color -> Cirkel ->  Picture
ball c (Cirkel (x,y) r) = Translate x y $ Color c $ circleSolid r
-}
