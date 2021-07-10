module GameFunctions
    (
    getActions,
    _A,
    _P,
    _H,
    drawGameTree,
    stepTree,
    drawMaybeGameTree,
    traverseTree,
    getInfoMap
    )
where

import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Tree as DT
import qualified Control.Monad.State as CMS

_H :: (Show action, Ord action) => (InformationMap action) -> DS.Set (History action)
_H infoMap = Prelude.foldl DS.union DS.empty (DM.elems infoMap)

maybeToSet :: Maybe a -> DS.Set a
maybeToSet Nothing = DS.empty
maybeToSet (Just e) = DS.singleton e

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just e) = [e]

showTreeElement :: (Show player, Show action) => TreeElement player action -> String
showTreeElement = show

drawGameTree :: (Show player, Show action) => DT.Tree (TreeElement player action) -> IO ()
drawGameTree gt = putStr $ DT.drawTree $ fmap showTreeElement gt

drawMaybeGameTree :: (Show player, Show action) => Maybe (DT.Tree (TreeElement player action)) -> IO ()
drawMaybeGameTree (Just gt) = drawGameTree gt
drawMaybeGameTree Nothing = return ()

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

-----------------------------------

{-
_Z :: (Eq action, Ord action) => (GameTree player action) -> DS.Set (History action)
_Z g = help DS.empty (subForest g) []
  where
  help set forest actions = foldl (moreHelp actions) set forest
  moreHelp actions set (action, Nothing) = DS.union set (DS.singleton (actions++[action]))
  moreHelp actions set (action, Just tree) = help set (subForest tree) (actions++[action])


getSetOfInfoSets :: (Ord action) => InformationMap action -> DS.Set (InformationSet action)
getSetOfInfoSets infoMap = DS.fromList $ DM.elems infoMap
_I :: (Ord action) => DS.Set (InformationSet action) -> History action -> Maybe (InformationSet action)
_I infoSets history = helper (DS.toList infoSets) history
  where
  helper []       _  = Nothing
  helper (is:iss) hs = if DS.member hs is then Just is else helper iss hs

-- Gets information sets of a given player
{-
_II_i :: (Show player, Show action, Ord action) =>
  (GameTree player action) -> DS.Set (InformationSet action) -> player -> DS.Set (InformationSet action)
_II_i g inSets i = DS.foldl (\outSets -> \infoSet -> help g i outSets infoSet) DS.empty inSets
  where
  help g i outSets infoSet = DS.foldl (\outInfoSet -> \h -> help2 g i h outSets) DS.empty infoSet
  help2 g i h infoSet = undefined --_P g
-}

-}
