import EventNetwork 
import Control.Monad.IO.Class
import Data.IORef 

main = 
  do env <- newEnv 
     (e, cb) <- newBaseE env
     
     e1 <- runEmits env (fmapThing (+1) e)
     e2 <- runEmits env (fmapThing (+1) e1)
     newIORef [e2]
     cb 1
     iteration env
     cb 1 
     iteration env

fmapThing f e = Await [Ex e] $
     do x <- checkNode e
        let v = fmap f x
        liftIO $ putStrLn (show v)
        pure $ Commit v (fmapThing f e)


