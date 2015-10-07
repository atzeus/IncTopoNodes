
{-# Language MagicHash #-} 
module IncTopoSort(TopoNode, Level, newNode, addEdge, removeEdge, getAliveParents, getLevel, isBefore,
                   PrioQueue, emptyPqueue, insertNode, dequeue) where

import Data.Int
import Data.Graph
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree
import System.Mem.Weak
import GHC.Prim
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.Trans
import Data.IntMap.Strict hiding (map,null,filter,insert)
import qualified Data.IntMap.Strict as IM

data TopoInfo = TopoNode {
   parents :: ![Weak (IORef TopoInfo)],
   info    :: !Any,
   level   :: {-# UNPACK #-} !Level }
  | BlackHole

newtype PrioQueue = PQ (IntMap TopoNode)

type Level = Int

newtype TopoNode = TN (IORef TopoInfo) deriving Eq

newtype TIORef a = TR (IORef TopoInfo) deriving Eq

newNode :: a -> IO (TopoNode, TIORef a)
newNode a = do r <- newIORef (TopoNode [] (unsafeCoerce# a) minBound)
               return (TN r, TR r)

readTIORef :: TIORef a -> IO a
readTIORef (TR r) = unsafeCoerce# . info <$> readIORef r

writeTIORef :: TIORef a -> a -> IO ()
writeTIORef (TR r) a = 
  do v <- readIORef r
     writeIORef r (v {info = unsafeCoerce# a}) 

addEdge :: TopoNode -> TopoNode -> IO Bool
addEdge from@(TN fr) (TN to) = 
  do toInfo  <- readIORef to
     notLoop <- ensureLevel (level toInfo + 1) from
     if notLoop 
     then do wTo  <- mkWeakIORef to (return ())  
             fref <- mkWeakIORef fr (removeEdgeWeak from wTo)
             writeIORef to (toInfo {parents = fref : parents toInfo} )
     else return ()
     return notLoop

ensureLevel :: Level -> TopoNode -> IO Bool
ensureLevel minLev n@(TN node) = 
  do ninfo <- readIORef node
     case ninfo of
      BlackHole -> return False
      ninfo    | level ninfo >= minLev ->  return True
      ninfo -> 
        do writeIORef node BlackHole
           pr <- catMaybes <$> mapM deRefWeak (parents ninfo)
           res <- mapM (ensureLevel (minLev + 1) . TN)  pr
           writeIORef node (ninfo {level = minLev})
           return (all id res)

removeEdgeWeak :: TopoNode -> Weak (IORef TopoInfo) -> IO ()
removeEdgeWeak from wTo = 
 do x <- deRefWeak wTo 
    case x of
      Just to -> removeEdge from (TN to) 
      Nothing -> return ()
      

removeEdge :: TopoNode -> TopoNode -> IO ()
removeEdge (TN from) (TN to) = 
  do toInfo <- readIORef to
     parents' <- loop (parents toInfo) 
     writeIORef to (toInfo {parents = parents'} ) where
  loop [] = return []
  loop (x : t) = 
      do s <- deRefWeak x
         case s of
           Just q  -> if q == to then loop t else (x :) <$> loop t
           Nothing -> loop t

getAliveParents :: TopoNode -> IO [TopoNode]
getAliveParents (TN n) = 
     do pr <- parents <$> readIORef n
        map TN . catMaybes <$> mapM deRefWeak pr

getLevel :: TopoNode -> IO Level
getLevel (TN n) = level <$> readIORef n

isBefore :: TopoNode -> TopoNode -> IO Bool
isBefore l r = (<) <$> getLevel l <*> getLevel r



newDullNode :: IO (TopoNode)
newDullNode = fst <$> newNode () 

emptyPqueue :: PrioQueue
emptyPqueue = PQ empty

insertNode :: TopoNode -> PrioQueue ->  IO PrioQueue
insertNode t (PQ pq) = do lev <- getLevel t 
                          return (PQ $ IM.insert lev t pq)

dequeue :: PrioQueue -> IO (Maybe (TopoNode, PrioQueue))
dequeue (PQ pq)
  | IM.null pq = return Nothing
  | otherwise  = 
     let ((lev,node),pq') = deleteFindMin pq
     in do lev' <- getLevel node
           if lev == lev' 
           then return (Just (node, PQ pq'))
           else dequeue (PQ $ IM.insert lev' node pq')
                                  


--- Below: Testing code

-- check concistency with Data.Graph toposort
propIsCorrect :: AdjList -> Property
propIsCorrect ad =  ioProperty $ 
  do let ad' = filterLoopEdges ad
     levels <- buildGraph ad
     let ord = (\i j -> (levels !! (i -1)) < levels !! (j - 1))
     let wrongs = check ord (toGraph ad')
     return (counterexample (show (wrongs, levels)) (null wrongs))


check :: (Int -> Int -> Bool) -> Graph -> [(Int,Int)]
check ord graph = concatMap check (dff graph) where
  check tree = concatMap (loop (rootLabel tree)) (subForest tree)
  loop parent tree = 
        let rest =  concatMap (loop (rootLabel tree)) (subForest tree)
        in if ord (rootLabel tree) parent
           then rest
           else (parent, rootLabel tree) : rest

toGraph :: AdjList -> Graph
toGraph (AdjList nr edges) = buildG (1,nr) edges


filterLoopEdges :: AdjList -> AdjList
filterLoopEdges (AdjList nr edges) = AdjList nr $ loop edges [] where
    loop (h : t) l | isLoopy (h : l) = loop t l
                   | otherwise       = loop t (h:l)
    loop []  l                       = l
    isLoopy edges = let g = buildG (1,nr) edges
                    in length (scc g)  < nr || any (selfreach g) [1..nr]
    selfreach g i = path g i i 


buildGraph :: AdjList -> IO [Level]
buildGraph (AdjList nr edges) = 
       do nodes <- sequence [newDullNode | i <- [1..nr]]
          mapM (\(from,to) -> addEdge (nodes !! (from -1)) (nodes !! (to - 1))) edges
          levels <- mapM getLevel nodes
          return (map (+ minBound) levels) -- get rid of these low values


data AdjList = AdjList { nrNodes :: Int,
                            edges :: [(Int,Int)] } deriving Show

instance Arbitrary AdjList where
  arbitrary = do nrNodes <- choose (0,200) 
                 nrEdges <- choose (0, nrNodes * 4)
                 AdjList nrNodes . sort <$> sequence
                       [ (,) <$> choose (1,nrNodes) <*> choose (1,nrNodes) | i <- [1..nrEdges]]

  shrink (AdjList nrNodes l) = map renumber (shrinkList (const []) l)
   where renumber :: [(Int,Int)] -> AdjList
         renumber [] = AdjList 0 []
         renumber l = AdjList nrNodes reslist
              where nrNodes = maximum (map fst reslist ++ map snd reslist)
                    reslist = map renumber l
                    renumber (x,y) = (renumberWith x, renumberWith y)
                    renumberWith j = j - length (takeWhile (< j) notPresent)
                    notPresent = sort $ filter (not . (`elem` usedNames)) [1..maximum usedNames]
                    usedNames = map fst l ++ map snd l

       
