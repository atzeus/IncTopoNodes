

main =
  mdo -- GUI code:
      (loadc, savec) <- row 10 $ (,) <$> button "load" <*> button "save"
      (clearc,undoc) <- row 10 $ (,) <$> button "clear" <*> button "undo" 
      mouseEvs       <- drawPanel (draw <$> model)

      -- loads and saves   
      loads <- liftNow $ plan (liftIO load <$ loadc)                 
      liftNow $ plan (liftIO . save <$> model <@< s)

      -- wire logic:
      let stateUpdates = 
         (const clean <$  clears ) `union` 
         (const       <$> loads )  `union`
         (undo        <$> undoc )  `union`
         (click       <$> clicks   mouseEvs ) `union`
         (release     <$> releases mouseEvs ) `union`
         (move        <$> moves    mouseEvs ) 
      model <- scan ($) clean stateUpdates
   
       
  


-------------------------------------------------------------------------

data State
  = State
  { linez   :: [(Point,Point)]
  , current :: Maybe (Point,Point)
  }
 deriving ( Eq, Show, Read )

clean :: State
clean = State{ linez = [], current = Nothing }

click :: Point -> State -> State
click p st = st{ current = Just (p,p) }

move :: Point -> State -> State
move p st = case current st of
              Nothing     -> st
              Just (p1,_) -> st{ current = Just (p1,p) }

release :: Point -> State -> State
release p st = case current st of
                 Nothing     -> st
                 Just (p1,_) -> st{ linez   = (p1,p) : linez st
                                  , current = Nothing
                                  }

clear :: State -> State
clear st = clean

undo :: State -> State
undo st = st{ linez = drop 1 (linez st) }

load :: IO (State -> State)
load =
  do ms <- try (readFile "drawing.txt")
     return (\st -> case ms of
                      Right s -> read s
                      _       -> st)

save :: State -> IO ()
save st = writeFile "drawing.txt" (show st)

draw :: State -> Drawing
draw st = drawings $
  [ Line p1 p2
  | (p1,p2) <- linez st
  ] ++
  [ Context (fgColor red) (Line p1 p2)
  | Just (p1,p2) <- [current st]
  ] 
 where
  red   = rgb 65535 0 0

-------------------------------------------------------------------------
