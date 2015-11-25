
{-# Language MagicHash,Rank2Types,ImpredicativeTypes, GADTs, KindSignatures #-} 
module IncTopoSort(Node,  ExNode(..), Level,  newNode, addEdge, removeEdge, getAliveParents, removeParents, getLevel, isBefore, eqConv, readNode, writeNode,
                   PrioQueue, emptyPqueue, isInQueue, insertNode, dequeue) where

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
import Unsafe.Coerce
import Control.Monad.Trans
import Data.IntMap.Strict hiding (map,null,filter,insert)
import qualified Data.IntMap.Strict as IM


data TopoInfo = TopoNode {
   parents :: ![Weak (IORef TopoInfo)],
   info    :: !Any,
   level   :: {-# UNPACK #-} !Level }
  | BlackHole

newtype PrioQueue f = PQ (IORef (IntMap [ExNode f])) deriving Eq

type Level = Int

data ExNode f where
  ExN :: Node (f a) -> ExNode f

newtype SomeNode (f :: * -> *) = SN (IORef TopoInfo) deriving Eq
newtype Node a = Node (IORef TopoInfo) deriving Eq

toExNode :: SomeNode f -> ExNode f
toExNode (SN n) = ExN (Node n)


eqConv :: Node (g a) -> Node (g b) -> Maybe (f a -> f b)
eqConv (Node x) (Node y) | x == y    = Just unsafeCoerce
                         | otherwise = Nothing

exNodeEq :: ExNode f -> ExNode f -> Bool
exNodeEq (ExN (Node x)) (ExN (Node y)) = x == y

toSomeNode :: Node (f a) -> SomeNode f
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

addEdge :: Node (f a) -> Node (f b) -> IO Bool
addEdge from to = addEdgeS (toSomeNode from) (toSomeNode to)

addEdgeS :: SomeNode f -> SomeNode f -> IO Bool
addEdgeS from@(SN fr) (SN to) = 
  do toInfo  <- readIORef to
     notLoop <- ensureLevelS (level toInfo + 1) from
     if notLoop 
     then do wTo  <- mkWeakIORef to (return ())  
             fref <- mkWeakIORef fr (removeEdgeWeak from wTo)
             writeIORef to (toInfo {parents = fref : parents toInfo} )
     else return ()
     return notLoop


ensureLevel :: Level -> Node (f a) -> IO Bool
ensureLevel minLev n = ensureLevelS minLev (toSomeNode n)

ensureLevelS :: Level -> SomeNode f -> IO Bool
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


removeEdgeWeak :: SomeNode f -> Weak (IORef TopoInfo) -> IO ()
removeEdgeWeak from wTo = 
 do x <- deRefWeak wTo 
    case x of
      Just to -> removeEdgeS from (SN to) 
      Nothing -> return ()

removeEdge :: Node (f a) -> Node (f b) -> IO ()
removeEdge from to = removeEdgeS (toSomeNode from) (toSomeNode to)

removeParents :: Node (f a) -> IO ()
removeParents from = removeParentsS (toSomeNode from)

removeParentsS :: SomeNode f -> IO ()
removeParentsS (SN to) =
  do info <- readIORef to
     writeIORef to (info { parents = [] })

removeEdgeS :: SomeNode f -> SomeNode f -> IO ()
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

getAliveParents :: Node (f a) -> IO [ExNode f]
getAliveParents (Node n) = 
     do pr <- parents <$> readIORef n
        map (toExNode . SN) . catMaybes <$> mapM deRefWeak pr

getLevel :: Node a -> IO Level
getLevel (Node n) = level <$> readIORef n

getLevelEx :: ExNode f -> IO Level
getLevelEx (ExN n) = getLevel n

getLevelS :: SomeNode f -> IO Level
getLevelS (SN n) = level <$> readIORef n

isBefore :: Node a -> Node b -> IO Bool
isBefore l r = (<) <$> getLevel l <*> getLevel r

data Id x = Id x

type NNode = SomeNode Id

newDullNode :: IO NNode
newDullNode = toSomeNode <$> newNode (Id ()) 

emptyPqueue :: IO (PrioQueue f)
emptyPqueue = PQ <$> newIORef empty

isInQueue :: ExNode f -> PrioQueue f -> IO Bool
isInQueue (ExN n) (PQ pqr) = 
   do pq <- readIORef pqr
      lev <- getLevel n
      return $ isJust $ 
          do l <- IM.lookup lev pq 
             find (exNodeEq (ExN n)) l

      
 

insertNode :: ExNode f -> PrioQueue f ->  IO ()
insertNode ex@(ExN n) (PQ pqr) = 
   do pq <- readIORef pqr
      lev <- getLevel n
      writeIORef pqr ( IM.insertWith (++) lev [ex] pq)

dequeue :: PrioQueue f -> IO (Maybe (Level, ExNode f))
dequeue (PQ pqr) =
  do pq <- readIORef pqr
     if IM.null pq 
     then return Nothing
     else let ((lev,node : t') ,pq') = deleteFindMin pq
          in do lev' <- getLevelEx node
                if lev == lev' 
                then do case t' of
                          [] -> writeIORef pqr pq'
                          _  -> writeIORef pqr (IM.insert lev t' pq')
                        return (Just (lev, node))
                else do writeIORef pqr ( IM.insertWith (++) lev' [node] pq')
                        dequeue (PQ pqr)
                                  


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

       
