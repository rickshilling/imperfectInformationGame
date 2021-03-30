module GameFunctions
    (
    gameTraverse,
    newGameTraverse,
    drawGameTree,
    putStrGameTree,
    putStrMaybeGameTree,
    getInformationMap,
    insertInfoMap,
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
import qualified Data.List as DL

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

newGameTraverse :: (Eq action) => GameTree player action -> NewHistory action -> Maybe (GameTree player action)
newGameTraverse g (NewHistory []) = Just g
newGameTraverse (GameNode p f) (NewHistory (a:as)) = do
  actionTreePair <- DL.find (\x -> fst x == a) f
  tree <- snd actionTreePair
  finalTree <- newGameTraverse tree (NewHistory as)
  return finalTree

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

getInformationMap :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> (InformationMap action)
getInformationMap tree = traverseHelp DM.empty (GameTypes.subForest tree) []
  where
  traverseHelp infoMap forest actions = Prelude.foldl (moreHelp actions) (insertInfoMap forest actions infoMap) forest
  moreHelp actions infoMap (action, Nothing) = DM.insertWith DS.union DS.empty (DS.singleton (actions++[action])) infoMap
  moreHelp actions infoMap (action, Just tree) = traverseHelp infoMap (GameTypes.subForest tree) (actions++[action])

getNewInfoMap :: (Ord action) => (GameTree player action) -> (NewInformationMap action)
getNewInfoMap g = traverseHelp DM.empty (GameTypes.subForest g) (NewHistory [])
  where
  traverseHelp infoMap forest history = Prelude.foldl (moreHelp2 history) (insertNewInfoMap forest history infoMap) forest
 
moreHelp2 :: (Ord action) => NewHistory action -> (NewInformationMap action) -> (action, Maybe (GameTree player action))) -> (NewInformationMap action)
moreHelp2 history infoMap (action, Nothing) = infoMap --DM.insertWith unionNewInfoSet (NewChoices DS.empty) (NewInformationMap DM.empty) --infoMap
  --moreHelp history infoMap (action, Nothing) = infoMap
  --moreHelp actionHistory infoMap (action, Nothing) = DM.insertWith unionNewInfoSet (NewChoices DS.empty) infoMap 
  --moreHelp actionHistory infoMap (action, Just tree) = infoMap
{-
getNewInfoMap :: (Ord action) => (GameTree player action) -> (NewInformationMap action)
getNewInfoMap g = foldl (\infoMap -> \pair -> help (NewHistory []) infoMap pair) DM.empty (subForest g)
  where
    help (NewHistory as) infoMap pair = DM.insertWith DS.union (getChoices )
-}

insertInfoMap :: (Show player, Show action, Ord action) =>
  [(action, Maybe (GameTree player action) )] -> [action] -> (InformationMap action) -> (InformationMap action)
insertInfoMap forest actions infoMap = DM.insertWith DS.union (getActions forest) (DS.singleton actions) infoMap

insertNewInfoMap :: (Ord action) =>
  [(action, Maybe (GameTree player action) )] -> NewHistory action -> (NewInformationMap action) -> (NewInformationMap action)
insertNewInfoMap forest history infoMap = DM.insertWith
  unionNewInfoSet
  (getNewChoices forest)
  (NewInformationSet (DS.singleton history)) -- (NewInformationSet DS.empty) --
  infoMap

is1 = NewInformationSet DS.empty
is2 = NewInformationSet $ DS.fromList [NewHistory ([1,2,3] :: [Int]) ,NewHistory ([4,5,6] :: [Int])]
c1  = NewChoices $ DS.fromList ([7,8,9] :: [Int])
c2  = NewChoices $ DS.fromList ([10,11,12] :: [Int])
im = (DM.empty) :: NewInformationMap Int

--imp  = DM.insertWith
--type NewInformationMap action = Map (NewChoices action) (NewInformationSet action)

unionNewChoice :: (Ord action) => NewChoices action -> NewChoices action -> NewChoices action
unionNewChoice a b = NewChoices (DS.union (choices a) (choices b))

unionNewInfoSet :: (Ord action) => NewInformationSet action -> NewInformationSet action -> NewInformationSet action
unionNewInfoSet a b = NewInformationSet $ (DS.union (infoSet a) (infoSet b))

getNewChoices :: (Ord action) => [(action, Maybe (GameTree player action))] -> NewChoices action
getNewChoices forest = NewChoices $ foldl (\set -> \e -> DS.union set (DS.singleton (fst e))) DS.empty forest

getActions :: (Show player, Show action, Ord action) =>
  [(action, Maybe (GameTree player action))] -> DS.Set action
getActions = Prelude.foldl (\set -> \element -> DS.union set (DS.singleton (fst element))) DS.empty

sameInfoSet :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> History action -> History action -> Bool
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
   (InformationMap action) -> DS.Set (History action)
_H infoMap = Prelude.foldl DS.union DS.empty (DM.elems infoMap)

_Z :: (Eq action, Ord action) =>
  (GameTree player action) -> DS.Set (History action)
_Z g = help DS.empty (subForest g) []
  where
  help set forest actions = foldl (moreHelp actions) set forest
  moreHelp actions set (action, Nothing) = DS.union set (DS.singleton (actions++[action]))
  moreHelp actions set (action, Just tree) = help set (subForest tree) (actions++[action])
