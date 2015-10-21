
{-# Language MagicHash #-} 
module IncTopoSort(Node, SomeNode, Level, newNode, addEdge, removeEdge, getAliveParents, getLevel, isBefore,
                   PrioQueue, emptyPqueue, insertNode, dequeue) where

import Data.Int
import Data.Graph hiding (Node) 
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree hiding (Node)
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

newtype PrioQueue = PQ (IntMap SomeNode)

type Level = Int

newtype SomeNode = SN (IORef TopoInfo) deriving Eq
newtype Node a = Node (IORef TopoInfo) deriving Eq

toSomeNode :: Node a -> SomeNode
toSomeNode (Node a) = SN a

newNode :: a -> IO (Node a)
newNode a = do r <- newIORef (TopoNode [] (unsafeCoerce# a) minBound)
               return (Node r)

readNode :: Node a -> IO a
readNode (Node r) = unsafeCoerce# . info <$> readIORef r

writeNode :: Node a -> a -> IO ()
writeNode (Node r) a = 
  do v <- readIORef r
     writeIORef r (v {info = unsafeCoerce# a}) 

addEdge :: Node a -> Node b -> IO Bool
addEdge from to = addEdgeS (toSomeNode from) (toSomeNode to)

addEdgeS :: SomeNode -> SomeNode -> IO Bool
addEdgeS from@(SN fr) (SN to) = 
  do toInfo  <- readIORef to
     notLoop <- ensureLevelS (level toInfo + 1) from
     if notLoop 
     then do wTo  <- mkWeakIORef to (return ())  
             fref <- mkWeakIORef fr (removeEdgeWeak from wTo)
             writeIORef to (toInfo {parents = fref : parents toInfo} )
     else return ()
     return notLoop


ensureLevel :: Level -> Node a -> IO Bool
ensureLevel minLev n = ensureLevelS minLev (toSomeNode n)

ensureLevelS :: Level -> SomeNode -> IO Bool
ensureLevelS minLev n@(SN node) = 
  do ninfo <- readIORef node
     case ninfo of
      BlackHole -> return False
      ninfo    | level ninfo >= minLev ->  return True
      ninfo -> 
        do writeIORef node BlackHole
           pr <- catMaybes <$> mapM deRefWeak (parents ninfo)
           res <- mapM (ensureLevelS (minLev + 1) . SN)  pr
           writeIORef node (ninfo {level = minLev})
           return (all id res)


removeEdgeWeak :: SomeNode -> Weak (IORef TopoInfo) -> IO ()
removeEdgeWeak from wTo = 
 do x <- deRefWeak wTo 
    case x of
      Just to -> removeEdgeS from (SN to) 
      Nothing -> return ()

removeEdge :: Node a -> Node b -> IO ()
removeEdge from to = removeEdgeS (toSomeNode from) (toSomeNode to)


removeEdgeS :: SomeNode -> SomeNode -> IO ()
removeEdgeS (SN from) (SN to) = 
  do toInfo <- readIORef to
     parents' <- loop (parents toInfo) 
     writeIORef to (toInfo {parents = parents'} ) where
  loop [] = return []
  loop (x : t) = 
      do s <- deRefWeak x
         case s of
           Just q  -> if q == to then loop t else (x :) <$> loop t
           Nothing -> loop t

getAliveParents :: Node a -> IO [SomeNode]
getAliveParents (Node n) = 
     do pr <- parents <$> readIORef n
        map SN . catMaybes <$> mapM deRefWeak pr

getLevel :: Node a -> IO Level
getLevel (Node n) = level <$> readIORef n

getLevelS :: SomeNode -> IO Level
getLevelS (SN n) = level <$> readIORef n

isBefore :: Node a -> Node b -> IO Bool
isBefore l r = (<) <$> getLevel l <*> getLevel r



newDullNode :: IO SomeNode
newDullNode = toSomeNode <$> newNode () 

emptyPqueue :: PrioQueue
emptyPqueue = PQ empty

insertNode :: SomeNode -> PrioQueue ->  IO PrioQueue
insertNode t (PQ pq) = do lev <- getLevelS t 
                          return (PQ $ IM.insert lev t pq)

dequeue :: PrioQueue -> IO (Maybe (SomeNode, PrioQueue))
dequeue (PQ pq)
  | IM.null pq = return Nothing
  | otherwise  = 
     let ((lev,node),pq') = deleteFindMin pq
     in do lev' <- getLevelS node
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
          mapM (\(from,to) -> addEdgeS (nodes !! (from -1)) (nodes !! (to - 1))) edges
          levels <- mapM getLevelS nodes
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

       
