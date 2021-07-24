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
    getInfoMap,
    _I
    )
where

import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Tree as DT
import qualified Control.Monad.State as CMS

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

_H :: (Show action, Ord action) => (InformationMap action) -> DS.Set (History action)
_H infoMap = Prelude.foldl DS.union DS.empty (DM.elems infoMap)

maybeToSet :: Maybe a -> DS.Set a
maybeToSet Nothing = DS.empty
maybeToSet (Just e) = DS.singleton e

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just e) = [e]

getActions :: (Show player, Show action, Ord action) => DT.Tree (TreeElement player action) -> DS.Set action
getActions gt = Prelude.foldl (\set -> \element -> DS.union set (maybeToSet (fromAction $ DT.rootLabel element))) DS.empty (DT.subForest gt)

_A :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History action -> Maybe (DS.Set action)
_A g h = (traverseTree g h) >>= (\tree -> return (getActions tree))

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


_I :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> InformationMap action -> History action ->Maybe (InformationSet action)
_I gt infoMap h = (traverseTree gt h) >>= (\t -> (DM.lookup (getActions t) infoMap))

_P :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History action -> Maybe player
_P g h = (traverseTree g h) >>= (\tree -> getPlayer $ DT.rootLabel tree)

_PP :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> (InformationSet action) -> Maybe player
_PP gt infoSet = undefined

--type History' action = [Maybe action]
--type InformationSet' action = Set (History' action)
--type InformationMap' action = Map (Set (Maybe action)) (InformationSet' action)
--type InformationMaps' player action = Map (Maybe player) (InformationMap' action)
getInfoMaps' :: (Ord action, Ord player) => DT.Tree (TreeElement player action) -> CMS.State (History' action, InformationMaps' player action) ()
getInfoMaps' (DT.Node element forest) = do
  (history', infoMaps') <- CMS.get                             -- (History' action, InformationMaps' player action)
  let player' = getPlayer element                                                -- Maybe player
  let action' = fromAction element                                               -- Maybe action
  let maybeInfoMap' = DM.lookup player' infoMaps'                                -- Maybe (InformationMap' action)
  let actionSet'' = DS.fromList $ Prelude.map (fromAction . DT.rootLabel) forest -- Set (Maybe action)
  let historySet'' = DS.fromList $ history' ++ [action']                         -- Set (History' action)
  let temp = \infoMap -> Just (DM.insertWith DS.union actionSet'' historySet'' infoMap)
  let maybeInfoMap2 = maybeInfoMap' >>= temp
  return ()
