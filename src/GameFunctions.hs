module GameFunctions
    (
    gameTraverse,
    drawGameTree,
    putStrGameTree,
    putStrMaybeGameTree,
    getInformationSets,
    insertInfoSet,
    getActions,
    sameInfoSet,
    _A,
    _P,
    _H,
    _Z
    ) where

import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS

gameTraverse :: (Eq action) =>
  (GameTree player action) -> [action] -> Maybe (GameTree player action)
gameTraverse t [] = Just t
gameTraverse (GameNode p []) (refA:as) = Nothing
gameTraverse (GameNode p ((currA, Nothing):rForests)) (refA:as) =
  if currA == refA then Nothing
  else gameTraverse (GameNode p rForests) (refA:as)
gameTraverse (GameNode p ((currA,Just mTree):rForests)) (refA:as) =
  if currA == refA then gameTraverse mTree as
  else gameTraverse (GameNode p rForests) (refA:as)

drawGameTree :: (Show player, Show action) =>
  (GameTree player action) -> [String]
drawGameTree (GameNode p forest) = (lines (show p)) ++ (drawSubTrees forest)
  where
    drawSubTrees [] = []
    drawSubTrees [(a, Nothing)] =  (show a) : []
    drawSubTrees [(a, Just gt)] =
      "|" : (show a) : shift "`-" "   " (drawGameTree gt)
    drawSubTrees ((a, Nothing):restForest) = (show a) : drawSubTrees restForest
    drawSubTrees ((a, Just gt):restForest) =
      "|" : (show a) : shift "+-" "|  " (drawGameTree gt) ++ (drawSubTrees restForest)
    shift first other = zipWith (++) (first : repeat other)

putStrGameTree :: (Show player, Show action) =>
  (GameTree player action)  -> IO ()
putStrGameTree g = putStr $ (unlines . drawGameTree) g

putStrMaybeGameTree :: (Show player, Show action) =>
  Maybe (GameTree player action) -> IO ()
putStrMaybeGameTree Nothing = putStrLn "Nothing"
putStrMaybeGameTree (Just gt) = putStrGameTree gt

getInformationSets :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> (InformationSets action)
getInformationSets tree = traverseHelp DM.empty (GameTypes.subForest tree) []
  where
  traverseHelp infoSet forest actions = Prelude.foldl (moreHelp actions) (insertInfoSet forest actions infoSet) forest
  moreHelp actions infoSet (action, Nothing) = DM.insertWith DS.union DS.empty (DS.singleton (actions++[action])) infoSet
  moreHelp actions infoSet (action, Just tree) = traverseHelp infoSet (GameTypes.subForest tree) (actions++[action])


insertInfoSet :: (Show player, Show action, Ord action) =>
  [(action, Maybe (GameTree player action) )] -> [action] -> (InformationSets action) -> (InformationSets action)
insertInfoSet forest actions infoSet = DM.insertWith DS.union (getActions forest) (DS.singleton actions) infoSet

getActions :: (Show player, Show action, Ord action) =>
  [(action, Maybe (GameTree player action))] -> DS.Set action
getActions = Prelude.foldl (\set -> \element -> DS.union set (DS.singleton (fst element))) DS.empty

sameInfoSet :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> [action] -> [action] -> Bool
sameInfoSet tree a1 a2 = helper (gameTraverse tree a1) (gameTraverse tree a2)
  where
  helper Nothing   Nothing   = True
  helper Nothing   _         = False
  helper _         Nothing   = False
  helper (Just t1) (Just t2) = (getActions (GameTypes.subForest t1)) == (getActions (GameTypes.subForest t2))

_A :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> History action -> Maybe (DS.Set action)
_A g h = (gameTraverse g h) >>= (\tree -> return (getActions (subForest tree)))

_P :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> History action -> Maybe player
_P g h = (gameTraverse g h) >>= (\tree -> return (rootLabel tree))

_H :: (Show action, Ord action) =>
   (InformationSets action) -> DS.Set (History action)
_H infoSet = Prelude.foldl DS.union DS.empty (DM.elems infoSet)

_Z :: (Eq action, Ord action) =>
  (GameTree player action) -> DS.Set (History action)
_Z g = help DS.empty (subForest g) []
  where
  help hSet forest actions = foldl (moreHelp actions) hSet forest
  --help hSet forest actions = foldl (\set -> \elem -> moreHelp actions set elem) hSet forest
  moreHelp actions set (action, Nothing) = DS.union set (DS.singleton (actions++[action]))
  moreHelp actions set (action, Just tree) = help set (subForest tree) (actions++[action])
