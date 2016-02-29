{-# Language RecursiveDo #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GlossFRP
import FRPLib
import AllReal
import Control.Monad.IO.Class()

main :: IO ()
main = glossFRP (InWindow "FRP!" (1024,800) (0,0))  white 60 mainFRP

type Time = Float

mainFRP :: Events Event -> Events Float -> Now (Behavior Picture)
mainFRP evs deltaTime =
  do time <- scan (+) 0 deltaTime

     let mouseMoves = filterJust $ getMouseMove <$> evs
     mousePos <- hold (-0,-0) mouseMoves

     let makeBall p = demo p evs mousePos time
     let rightClicks = filtere (isClick RightButton) evs
     let clicksAt = mousePos `at` rightClicks
     let newBalls = observeE $ makeBall <$> clicksAt
     balls <- dynList newBalls

     return $ Pictures <$> balls



demo :: Vec -> Events Event -> Behavior Vec -> Behavior Float -> Now (Behavior Picture)
demo ipos evs mousePos time =
    let radiusAt t = 35 + 25 * sin (2 * t)
        chooseColor True = green
        chooseColor False = red
         --colorb = chooseColor <$> mouseOver

    in mdo tstart <- sample time
           let localTime = (\x -> x - tstart) <$> time
           isDragging <- hold False dragUpdates
           let colorb = chooseColor <$> isDragging
           let enterMouse = when mouseOver
           mouseDt <- fromDeltas (toDeltas mousePos `during` isDragging)
           nrEnters <- scan (\x _ -> x + 1) 0 enterMouse
           let mkRadius n = 10 + 4 * n
           let radiusb = radiusAt <$> localTime
           --let radiusb = mkRadius <$> nrEnters
           let cirkelb = Cirkel <$> ((\x -> x .+ ipos) <$> mouseDt) <*> radiusb
           let mouseOver = isInside <$> mousePos <*>. cirkelb
           let leftClick = filtere (isClick LeftButton) evs
           let startDrag = leftClick `during` mouseOver
           let stopDrag = filtere (isRelease LeftButton) evs
           let dragUpdates = (True <$ startDrag) `union` (False <$ stopDrag)
           return $ ball <$> colorb <*> cirkelb


toDeltas :: Behavior Vec -> Events Vec
toDeltas b = (.-) <$> updates b >@>> prev b

fromDeltas :: Events Vec -> Now (Behavior Vec)
fromDeltas e = scan (\x y -> x .+ y) (0,0) e
{-
(<$) :: Functor f => a -> f b -> f a
x <$ f =
 fmap (const x) f
-}



ball :: Color -> Cirkel ->  Picture
ball c (Cirkel (x,y) r) = Translate x y $ Color c $ circleSolid r

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

type Vec = (Float,Float)

data Cirkel = Cirkel { center :: Vec, radius :: Float }

lerpColor :: Color ->  Color -> Float -> Color
lerpColor c  d f = makeColor (lerp r f r') (lerp g f g') (lerp b f b') (lerp a f a')
  where
        (r,g,b,a) = rgbaOfColor c
        (r',g',b',a') = rgbaOfColor d

lerp :: Float -> Float -> Float -> Float
lerp a i b = (1 - i') * a + i' * b
  where i' = max 0 (min 1 i)

isInside :: Vec -> Cirkel -> Bool
isInside p1 (Cirkel p2 r) = distance p1 p2 <= r

distance :: Vec -> Vec -> Float
distance p p2 = norm $ p2 .- p

(.+) :: Vec -> Vec -> Vec
(x,y) .+ (x',y') = (x + x' , y + y')

(.-) :: Vec -> Vec -> Vec
(x,y) .- (x',y') = (x - x' , y - y')

norm :: Vec -> Float
norm (x,y) = sqrt $ x * x + y * y
