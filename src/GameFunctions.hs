module GameFunctions
    (
    gameTraverse,
    --drawGameTree,
    --putStrGameTree,
    --putStrMaybeGameTree,
    --getInformationMap,
    --insertInfoMap,
    getActions,
    --sameInfoSet,
    _A,
    --_AofI,
    _P,
    _H,
    --_Z,
    getSetOfInfoSets,
    _I,
    --filterInfoSetByPlayer,
    pureSet,
    drawGameTree,
    --compareAction,
    stepTree,
    drawMaybeGameTree,
    traverseTree,
    buildInfoMap,
    getInfoMap
    ) where

import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Tree as DT

import qualified Control.Monad.State as CMS
--import Control.Monad.State

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
{-
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
-}

{-
getInformationMap :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> (InformationMap action)
getInformationMap tree = traverseHelp DM.empty (GameTypes.subForest tree) []
  where
  traverseHelp infoMap forest actions = Prelude.foldl (moreHelp actions) (insertInfoMap forest actions infoMap) forest
  moreHelp actions infoMap (action, Nothing) = DM.insertWith DS.union DS.empty (DS.singleton (actions++[action])) infoMap
  moreHelp actions infoMap (action, Just tree) = traverseHelp infoMap (GameTypes.subForest tree) (actions++[action])

insertInfoMap :: (Show player, Show action, Ord action) =>
  [(action, Maybe (GameTree player action) )] -> [action] -> (InformationMap action) -> (InformationMap action)
insertInfoMap forest actions infoMap = DM.insertWith DS.union (getActions forest) (DS.singleton actions) infoMap

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

_AofI :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> InformationSet action -> Maybe (DS.Set action)
_AofI g infoSet = _A g (DS.elemAt 0 infoSet)
-}


_H :: (Show action, Ord action) => (InformationMap action) -> DS.Set (History action)
_H infoMap = Prelude.foldl DS.union DS.empty (DM.elems infoMap)
{-
_Z :: (Eq action, Ord action) => (GameTree player action) -> DS.Set (History action)
_Z g = help DS.empty (subForest g) []
  where
  help set forest actions = foldl (moreHelp actions) set forest
  moreHelp actions set (action, Nothing) = DS.union set (DS.singleton (actions++[action]))
  moreHelp actions set (action, Just tree) = help set (subForest tree) (actions++[action])
-}
getSetOfInfoSets :: (Ord action) => InformationMap action -> DS.Set (InformationSet action)
getSetOfInfoSets infoMap = DS.fromList $ DM.elems infoMap

_I :: (Ord action) => DS.Set (InformationSet action) -> History action -> Maybe (InformationSet action)
_I infoSets history = helper (DS.toList infoSets) history
  where
  helper []       _  = Nothing
  helper (is:iss) hs = if DS.member hs is then Just is else helper iss hs

-- Gets information sets of a given player
_II_i :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> DS.Set (InformationSet action) -> player -> DS.Set (InformationSet action)
_II_i g inSets i = DS.foldl (\outSets -> \infoSet -> help g i outSets infoSet) DS.empty inSets
  where
  help g i outSets infoSet = DS.foldl (\outInfoSet -> \h -> help2 g i h outSets) DS.empty infoSet
  help2 g i h infoSet = undefined --_P g
{-
filterInfoSetByPlayer :: (Show player, Show action, Ord action, Eq player) =>
  (GameTree player action) -> player -> InformationSet action -> InformationSet action
filterInfoSetByPlayer g p infoSet = DS.filter (\h -> (_P g h) == (Just p)) infoSet
-}
pureSet :: Maybe (DS.Set a) -> DS.Set a
pureSet Nothing = DS.empty
pureSet (Just set) = set

maybeToSet :: Maybe a -> DS.Set a
maybeToSet Nothing = DS.empty
maybeToSet (Just e) = DS.singleton e

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just e) = [e]
--

showTreeElement :: (Show player, Show action) => TreeElement player action -> String
showTreeElement = show

drawGameTree :: (Show player, Show action) => DT.Tree (TreeElement player action) -> IO ()
drawGameTree gt = putStr $ DT.drawTree $ fmap showTreeElement gt

drawMaybeGameTree :: (Show player, Show action) => Maybe (DT.Tree (TreeElement player action)) -> IO ()
drawMaybeGameTree (Just gt) = drawGameTree gt
drawMaybeGameTree Nothing = return ()

{-
compareAction :: (Eq action) => DT.Tree (TreeElement player action) -> action -> Maybe Bool
compareAction t a = liftM (== a) (fromAction $ DT.rootLabel t)
-}

stepTree :: (Eq action) => DT.Tree (TreeElement player action) -> action -> Maybe (DT.Tree (TreeElement player action))
stepTree gt a = DL.find (\t -> (Just a == (fromAction $ DT.rootLabel t)) ) (DT.subForest gt)

traverseTree :: (Eq action) => DT.Tree (TreeElement player action) -> [action] -> Maybe (DT.Tree (TreeElement player action))
traverseTree gt [] = Just gt
traverseTree gt (a:as) = (stepTree gt a) >>= (\t -> traverseTree t as)

_A :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History action -> Maybe (DS.Set action)
_A g h = (traverseTree g h) >>= (\tree -> return (getActions tree))

_P :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History action -> Maybe player
_P g h = (traverseTree g h) >>= (\tree -> getPlayer $ DT.rootLabel tree)

getActions :: (Show player, Show action, Ord action) => DT.Tree (TreeElement player action) -> DS.Set action
getActions gt = Prelude.foldl (\set -> \element -> DS.union set (maybeToSet (fromAction $ DT.rootLabel element))) DS.empty (DT.subForest gt)

buildInfoMap :: (Ord action) => DT.Tree (TreeElement player action) -> CMS.State ([Maybe action], DM.Map (DS.Set (Maybe action)) (DS.Set [Maybe action])) ()
buildInfoMap (DT.Node element forest) = do
  (value, infoMap) <- CMS.get
  let value' = value ++ [fromAction element]
  let key' = DS.fromList $ Prelude.map (fromAction . DT.rootLabel) forest
  let infoMap' = DM.insertWith DS.union key' (DS.singleton value') infoMap
  CMS.put (value', infoMap')
  _ <- mapM buildInfoMap forest
  (_,infoMap'') <- CMS.get
  CMS.put (value, infoMap'')
  return ()

getTemp :: Ord action => Game player action -> Int
getTemp game = 5
  where
    tt = fromAction . DT.rootLabel . getTree $ game

getInfoMap :: (Ord action) => DT.Tree (TreeElement player action) -> CMS.State (History action, InformationMap action) ()
getInfoMap (DT.Node element forest) = do
  (value, infoMap) <- CMS.get
  let value' = value ++ (maybeToList . fromAction $ element)
  let selectedForest = Prelude.filter (\te -> (fromAction. DT.rootLabel $ te) /= Nothing) forest
  let selectedMaybeActions = Prelude.map (fromAction . DT.rootLabel ) selectedForest
  let selectedActions = DS.fromList $ Prelude.map (\(Just x) -> x) selectedMaybeActions
  let key' = selectedActions
  let infoMap' = DM.insertWith DS.union key' (DS.singleton value') infoMap
  CMS.put (value', infoMap')
  _ <- mapM getInfoMap forest
  (_,infoMap'') <- CMS.get
  CMS.put (value, infoMap'')
  return ()

