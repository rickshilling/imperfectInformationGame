module GameFunctions
    (
    _A,
    _P,
    _H,
    drawGameTree,
    stepTree,
    drawMaybeGameTree,
    traverseTree,
    getInfoMaps,
    getInfoMapsState,
    _I
    )
where

import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Tree as DT
import qualified Control.Monad.State as CMS
import Debug.Trace

showTreeElement :: (Show player, Show action) => TreeElement player action -> String
showTreeElement = show

drawGameTree :: (Show player, Show action) => DT.Tree (TreeElement player action) -> IO ()
drawGameTree gt = putStr $ DT.drawTree $ fmap showTreeElement gt

drawMaybeGameTree :: (Show player, Show action) => Maybe (DT.Tree (TreeElement player action)) -> IO ()
drawMaybeGameTree (Just gt) = drawGameTree gt
drawMaybeGameTree Nothing = return ()

stepTree :: (Eq action) => DT.Tree (TreeElement player action) -> (Maybe action) -> Maybe (DT.Tree (TreeElement player action))
stepTree gt a = DL.find (\t -> (a == (fromAction $ DT.rootLabel t)) ) (DT.subForest gt)

traverseTree :: (Eq action) => DT.Tree (TreeElement player action) -> [Maybe action] -> Maybe (DT.Tree (TreeElement player action))
traverseTree gt [] = Just gt
traverseTree gt (a:as) = (stepTree gt a) >>= (\t -> traverseTree t as)

_H :: (Show action, Ord action) => (InformationMap' action) -> DS.Set (History' action)
_H infoMap = Prelude.foldl DS.union DS.empty (DM.elems infoMap)

maybeToSet :: Maybe a -> DS.Set a
maybeToSet Nothing = DS.empty
maybeToSet (Just e) = DS.singleton e

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just e) = [e]

maybeMapToMap :: Maybe (DM.Map k a) -> DM.Map k a
maybeMapToMap Nothing = DM.empty
maybeMapToMap (Just map') = map'

getActions :: (Show player, Show action, Ord action) => DT.Tree (TreeElement player action) -> DS.Set (Maybe action)
getActions gt = Prelude.foldl (\set -> \element -> DS.union set (DS.singleton $ fromAction $ DT.rootLabel element)) DS.empty (DT.subForest gt)

_A :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History' action -> Maybe (DS.Set (Maybe action))
_A g h = (traverseTree g h) >>= (\tree -> return (getActions tree))

_I :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> InformationMap' action -> History' action ->Maybe (InformationSet' action)
_I gt infoMap h = (traverseTree gt h) >>= (\t -> (DM.lookup (getActions t) infoMap))

_P :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> History' action -> Maybe player
_P g h = (traverseTree g h) >>= (\tree -> getPlayer $ DT.rootLabel tree)

{-
_PP :: (Ord action, Show player, Show action) => DT.Tree (TreeElement player action) -> (InformationSet action) -> Maybe player
_PP gt infoSet = undefined
-}

getInfoMapsState :: (Ord action, Ord player) => DT.Tree (TreeElement player action) -> CMS.State (History' action, InformationMaps' player action) ()
getInfoMapsState (DT.Node element forest) = do
  (history', infoMaps') <- CMS.get
  let player' = getPlayer element
  let action' = fromAction element
  let maybeInfoMap' = DM.lookup player' infoMaps'
  let actionSet'' = DS.fromList $ Prelude.map (fromAction . DT.rootLabel) forest 
  let history'' = history' ++ [action']
  let historySet'' = DS.singleton $ history''
  let infoMap' = maybeMapToMap $ DM.lookup player' infoMaps'
  let infoMap'' = DM.insertWith DS.union actionSet'' historySet'' infoMap'
  let infoMaps'' = DM.insert player' infoMap'' infoMaps'
  CMS.put (history'', infoMaps'')
  _ <- mapM getInfoMapsState forest
  (_,infoMaps''') <- CMS.get
  CMS.put (history', infoMaps''')
  return ()

getInfoMaps :: (Ord action, Ord player) => DT.Tree (TreeElement player action) -> InformationMaps' player action
getInfoMaps gt = snd $ snd $ CMS.runState (getInfoMapsState gt) ([],DM.empty)

