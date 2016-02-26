{-# Language OverloadedStrings,RecursiveDo #-} 


import Goey
import FRPLib
import WxFRP
import Data.Maybe
import qualified Graphics.UI.WX as WX

main :: IO ()
main = runGoey example1


example1 = 
 mdo click <- button2 [ text := "click me!", enabled := enable]
     enable <- checkBox [] False 
     l <- scan (\x _ -> x + 1) 0 (when enable click)  
     label (show <$> l)
     

parseIt :: Double -> Goey (Step Double)
parseIt d = 
  mdo  s <- entry  [bgcolor := color] (show d)
       let parse = readDouble <$> s
       let color = chooseColor . isJust <$> parse
       return (fromMaybe 0 <$> parse)
 where chooseColor True = WX.white
       chooseColor False = WX.red

entries :: [Double] -> Goey (Step [Double])
entries d = sequenceA <$> mapM (parseIt) d

example :: Goey ()
example = 
 mdo let actions :: Events ([Double] -> [Double] )
         actions = ((0 :) <$ a) `union` (drop 1 <$ r)
     let newModels :: Events [Double]
         newModels = actions >@> prev model
     let newGuis :: Events (Goey (Step [Double]))
         newGuis = entries <$> newModels
     (a,r) <- row 10 $ (,) <$> button "add" <*> button "remove"
     model <- dyn (return (pure [])) newGuis
     label (show . sum <$> model)
     return ()

readDouble :: String -> Maybe Double
readDouble "" = Just 0
readDouble l  = readMaybe l

readMaybe :: Read a => String -> Maybe a
readMaybe s = listToMaybe [x | (x,"") <- reads s]
