
{-# Language MagicHash,Rank2Types,ImpredicativeTypes, GADTs, KindSignatures #-} 
module IncTopoSort(
     -- $intro
     -- * Mutable cell operations
     Node, 
     newNode, readNode, writeNode, modifyNode, modifyNode', heqNode,
     -- * Ordering operations
      Ex(..), ExNode(..), Level,  getLevel, isBefore,  addEdge, removeEdge, getParents, ensureLevel, removeIngoingEdges, ensureAfter,
     -- * Priority queue
     PrioQueue,isEmptyPqueue, emptyPqueue, insertPQ, scheduleParents, dequeue) where

import Data.Int
import Data.Graph hiding (Node) 
import Data.IORef
import Ex
import Data.List
import Data.Maybe
import Data.Tree hiding (Node)
import System.Mem.Weak
import GHC.Prim
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Unsafe.Coerce
import Control.Monad.Trans
import qualified Data.PQueue.Prio.Min as IM


{-$intro

This module offers an incremental topological sort of a directed acyclic graph. 

A topological sort gives a mapping @level :: Node -> Int@ such that if there is a path from @b@ to @a@ then @level b >= level a@.

It is intended to implement pull-based FRP. Each node stores only a weak references to its parent (and no references to its children).

This module is not thread-safe.

For efficiency, each node also functions as a mutable cell.
-}


data TopoInfo = TopoNode {
   parents :: ![Weak (IORef TopoInfo)],
   info    :: !Any,
   level   :: {-# UNPACK #-} !Level }
  | BlackHole


type Level = Int

getLevel :: Node f a -> IO Level
getLevel (Node n) = level <$> readIORef n


isBefore :: Node f a -> Node f b -> IO Bool
isBefore l r = (<) <$> getLevel l <*> getLevel r

heqNode :: Node f a -> Node f b -> Bool
heqNode (Node l) (Node r) = l == r

{-| A node in the graph, containing a mutable cell for values of type @f a@.
-}

newtype Node (f :: * -> *) a = Node (IORef TopoInfo) deriving Eq



data ExNode f where
  ExNode :: Node f a -> ExNode f




newNode ::  f a -> IO (Node f a)
newNode a = do r <- newIORef (TopoNode [] (unsafeCoerce# a) minBound)
               return (Node r)

readNode :: Node f a -> IO (f a)
readNode (Node r) = unsafeCoerce# . info <$> readIORef r

writeNode :: Node f a -> f a -> IO ()
writeNode (Node r) a = 
  do v <- readIORef r
     writeIORef r (v {info = unsafeCoerce# a}) 

modifyNode :: Node f a -> (f a -> f a) -> IO ()
modifyNode n f = do x <- readNode n
                    let y = f x
                    writeNode n y


modifyNode' :: Node f a -> (f a -> f a) -> IO ()
modifyNode' n f = do x <- readNode n
                     let y = f x
                     y `seq` writeNode n y

{-| @addEdge from to@ creates an edge from @from@ to @to@. Returns @False@ if the edge could not be added because this edge would cause a loop. In case the edge already exists, nothing is done.
-}
addEdge :: Node f a -> Node f b -> IO Bool
addEdge from@(Node fr) to@(Node tor) = 
      do notLoop <- ensureAfter from to
         if notLoop 
         then do toInfo  <- readIORef tor
                 wTo  <- mkWeakIORef tor (return ())  
                 fref <- mkWeakIORef fr (removeEdgeWeak from wTo)
                 writeIORef tor (toInfo {parents = fref : parents toInfo} )
         else return ()
         return notLoop

{-| @edgeExists from to@ returns wether the edge @(from,to)@ is in the dag. 
-}
edgeExists :: Node f a -> Node f b -> IO Bool
edgeExists (Node from) (Node to) = 
  do toInfo <- readIORef to
     loop (parents toInfo) where
  loop [] = return False
  loop (x : t) = 
      do s <- deRefWeak x
         case s of
           Just q  | q == from -> return False
           _ -> loop t

ensureAfter :: Node f a -> Node f b -> IO Bool
ensureAfter from@(Node fr) to@(Node tor) = 
      do toInfo  <- readIORef tor
         ensureLevel' (level toInfo + 1) from




ensureLevel' :: Level -> Node f a -> IO Bool
ensureLevel' minLev n@(Node node) = 
  do ninfo <- readIORef node
     case ninfo of
      BlackHole -> return False
      ninfo    | level ninfo >= minLev ->  return True
      ninfo -> 
        do writeIORef node BlackHole
           pr <- catMaybes <$> mapM deRefWeak (parents ninfo)
           res <- mapM (ensureLevel' (minLev + 1) . Node)  pr
           writeIORef node (ninfo {level = minLev})
           return (all id res)

removeEdgeWeak :: Node f a -> Weak (IORef TopoInfo) -> IO ()
removeEdgeWeak from wTo = 
 do x <- deRefWeak wTo 
    case x of
      Just to -> removeEdge from (Node to) 
      Nothing -> return ()


{-| Raises the level of the node to at least be the given level. This may influence the levels in the rest of the graph as well.
-}
ensureLevel :: Level -> Node f a -> IO ()
ensureLevel l n = ensureLevel' l n >> return ()



{-| Removes all incoming edges to this node (i.e. edges from parents to this node).
-}
removeIngoingEdges :: Node f a -> IO ()
removeIngoingEdges (Node to) =
  do info <- readIORef to
     writeIORef to (info { parents = [] })


{-| @removeEdge from to@ removes an edge from @from@ to @to@.
-}
removeEdge :: Node f a -> Node f b -> IO ()
removeEdge (Node from) (Node to) = 
  do toInfo <- readIORef to
     parents' <- loop (parents toInfo) 
     writeIORef to (toInfo {parents = parents'} ) where
  loop [] = return []
  loop (x : t) = 
      do s <- deRefWeak x
         case s of
           Just q  -> if q == to then pure t else (x :) <$> loop t
           Nothing -> loop t

{-| Gives all parent nodes of the given node (who have not been garbage collected).
-}
getParents :: Node f a -> IO [ExNode f]
getParents (Node n) = 
     do pr <- parents <$> readIORef n
        map (ExNode . Node) . catMaybes <$> mapM deRefWeak pr

{-| A priority queue which gives nodes in order of the partial order defined by the dag. The priority queue containing 'ExNode' elements of type @ExNode f@. Note that the order of
    the elements is always corresponds with the dag from which the nodes originate, even when their order changes after inserting them.
-}
newtype PrioQueue f = PQ (IORef (IM.MinPQueue Int (ExNode f)) ) deriving Eq


emptyPqueue :: IO (PrioQueue f)
emptyPqueue = PQ <$> newIORef IM.empty

isEmptyPqueue :: PrioQueue f -> IO Bool
isEmptyPqueue (PQ q) = null <$> readIORef q

scheduleParents :: PrioQueue f -> Node f x ->  IO ()
scheduleParents pq n = 
  do ps <- getParents n
     mapM_ (\(ExNode p) -> insertPQ pq p) ps
      

insertPQ :: PrioQueue f -> Node f x ->  IO ()
insertPQ (PQ pqr) n = 
   do pq <- readIORef pqr
      lev <- getLevel n
      writeIORef pqr ( IM.insert lev (ExNode n) pq)

dequeue :: PrioQueue f -> IO (Maybe (ExNode f))
dequeue (PQ pqr) =
  do pq <- readIORef pqr
     case IM.minViewWithKey pq of
       Nothing -> return Nothing 
       Just ( (lev, n@(ExNode node)), pq') -> 
          do lev' <- getLevel node
             if lev == lev' 
             then do writeIORef pqr pq'
                     return (Just n)
                else do writeIORef pqr (IM.insert lev' n  pq')
                        dequeue (PQ pqr)

 
                                  


--- Below: Testing code


type NNode = Node Id ()

data Id a = Id a

newDullNode :: IO NNode
newDullNode = newNode (Id ())

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

      
