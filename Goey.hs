{-# Language OverloadedStrings,FlexibleInstances,TypeSynonymInstances, RecursiveDo, ExistentialQuantification, ScopedTypeVariables,GeneralizedNewtypeDeriving #-} 


module Goey  where

import Graphics.UI.WXCore (getTextExtent)
import qualified Graphics.UI.WX as WX
import WxFRP
import FRPLib
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad
import Data.IORef
import Graphics.UI.WXCore.WxcClassTypes(WxObject(..))
import GHC.Exts( IsString(..) )

instance IsString (Step String) where
  fromString = pure

newtype Goey a = Goey (ReaderT (WX.Panel ()) (WriterT ([WX.Layout],[WxExt]) Now) a) deriving (Functor,Applicative,Monad,MonadFix)

data WxExt = forall a. WxExt (WxObject a)

newtype Attr w a = Attr (WX.Attr w a)
data Prop w = forall a . (Attr w a) := (Step a)

class Textual w where
  text :: Attr w String

class Able w where
  enabled :: Attr w Bool

class Colored w where
  color :: Attr w Color
  bgcolor :: Attr w Color

type Button = WX.Button ()
type Color = WX.Color
type TextCtrl = WX.TextCtrl ()

instance Textual Button where text = Attr WX.text
instance Able Button where enabled = Attr WX.enabled
instance Colored Button where color = Attr WX.color; bgcolor = Attr WX.bgcolor
instance Able TextCtrl where enabled = Attr WX.enabled
instance Colored TextCtrl where color = Attr WX.color; bgcolor = Attr WX.bgcolor

type CheckBox = WX.CheckBox	 ()

instance Textual CheckBox where text = Attr WX.text
instance Able CheckBox where enabled = Attr WX.enabled
instance Colored CheckBox where color = Attr WX.color; bgcolor = Attr WX.bgcolor

defaultLayout :: [WX.Layout] -> WX.Layout
defaultLayout [x] = x
defaultLayout [] = WX.glue
defaultLayout l = WX.column 10 l

setProp ::  w -> Prop w ->  Now ()
setProp w (Attr a := s) = setSink a s w

button2 :: [Prop Button] -> Goey (Events ())
button2 ps  = Goey $ 
   do w <- ask
      b <- liftIO $ WX.button w []
      lift $ lift $ mapM_ (setProp b) ps
      tell ([WX.widget b],[WxExt b])
      liftNow $ setSource0 WX.command b

data Drawing = Line Point Point
             | Drawings [Drawing]

drawing :: Step [Drawing] -> Goey (Events EventMouse)


entry ::  [Prop TextCtrl] -> String -> Goey (Step String)
entry ps s =  Goey $ 
   do w <- ask
      b <- liftIO $ WX.entry w []
      lift $ lift $ mapM_ (setProp b) ps
      liftIO $ WX.set b [WX.text WX.:= s]
      tell ([WX.widget b],[WxExt b])
      e <- liftNow $ setSourceAttr WX.update WX.text b
      liftNow $ step s e
  
checkBox :: [Prop CheckBox] -> Bool -> Goey (Step Bool)
checkBox ps  s = Goey $ 
   do w <- ask
      b <- liftIO $ WX.checkBox w []
      lift $ lift $ mapM_ (setProp b) ps
      liftIO $ WX.set b [WX.checked WX.:= s]
      tell ([WX.widget b],[WxExt b])
      e <- liftNow $ setSourceAttr WX.command WX.checked b
      liftNow $ step s e


runGoey :: Goey () -> IO ()
runGoey (Goey m) = runWXFrp $
  do 
     f <- liftIO $ WX.frame  [ WX.text        WX.:= "Goey!"]   
     p <- liftIO $ WX.panel f []
     (_,(ls,_)) <-  runWriterT (runReaderT m p)

     liftIO $ WX.set p [WX.layout WX.:=  defaultLayout ls]                          -- draw in a panel
     return ()
 
row :: Int -> Goey a -> Goey a
row space (Goey m) = Goey $ 
   do w <- ask
      (a,(ls,d)) <-  lift $ lift $ runWriterT (runReaderT m w)
      tell ([ WX.row space ls],d)
      return a

column :: Int -> Goey a -> Goey a
column space (Goey m) = Goey $ 
   do w <- ask
      (a,(ls,d)) <-  lift $ lift $ runWriterT (runReaderT m w)
      tell ([WX.column space ls],d)
      return a

instance BehaviorLike Goey where
  liftB b = Goey $ lift $ lift $ liftB b
  
liftNow :: Now a -> ReaderT (WX.Panel ()) (WriterT ([WX.Layout],[WxExt]) Now) a 
liftNow m =  lift $ lift m

button :: Step String -> Goey (Events ())
button textb  = Goey $ 
   do w <- ask
      b <- liftIO $ WX.button w []
      tell ([WX.widget b],[WxExt b])
      liftNow $ setSink WX.text textb b
      liftNow $ setSource0 WX.command b

label :: Step String -> Goey ()
label textb = Goey $ 
  do w <- ask
     b <- liftIO $ WX.staticText w []
     tell ([ WX.widget b],[WxExt b])
     liftNow $ setSink WX.text textb b



getTimePassed :: Goey (Step Time)
getTimePassed = Goey $ 
 do w <- ask
    i <- liftNow $ liftB $ time
    e <- liftNow $ getIdle w
    lift $ lift $ step i e

dyn :: Goey (Step a) -> Events (Goey (Step a)) -> Goey (Step a)
dyn ig eg = joinStep <$> dynG ig eg

dynG :: Goey a -> Events (Goey a) -> Goey (Step a)
dynG ig eg = do gg <- step ig eg
                dynS gg

dynS ::  Step (Goey a) -> Goey (Step a)
dynS m = Goey $
       do  w <- ask
           p <- liftIO $ WX.panel w []
           ig <- lift $ lift $ liftB $ prev m
           r <- liftIO $ newIORef []
           i <- lift $ lift $ runGoey r p ig
           u <- lift $ lift $ plan (runGoey r p <$> updates m)

           tell ([WX.fill $ WX.dynamic $ WX.widget p],[WxExt p])
           lift $ lift $ liftB $ stepB i u
  where runGoey :: IORef [WxExt] -> WX.Panel () -> Goey a -> Now a
        runGoey r p (Goey ig) = 
         do 
            (a,(ls,ex)) <-  runWriterT (runReaderT ig p)

            del <- liftIO$ readIORef r 
            liftIO $ mapM_ (\(WxExt x) -> WX.objectDelete x) del
            liftIO $  WX.set p [WX.layout WX.:= defaultLayout ls ]
            liftIO $ writeIORef r ex
            return a

dyns ::  Step (Goey (Step a)) -> Goey (Step a)
dyns m = join <$> dynS m





{-
example :: Goey ()
example =
  do  (cl, cr) <- row 10 $ (,) <$> button (pure "Left!") <*> button (pure "Right!")
      let evs = ('l' <$ cl) `unionl` ('r' <$ cr)
      time <- getTimePassed 
      s <- buffer 5 time evs
      ---let message i = "You've clicked " ++ show i ++ " time" ++ (if i >= 2 then "s" else "")
      label (show . fmap snd <$> s )
      return ()
-}


