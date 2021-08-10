module GameExample
  (
  )where

import GameTypes
import GameFunctions
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Tree as DT
--import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import Data.Map.Internal.Debug

data Player = P1 | P2 | Chance deriving (Eq,Ord,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show)

gt =
  DT.Node (TreeElement (Just Chance) Nothing) [
  DT.Node (TreeElement (Just P1)     (Just Heads)) [
      DT.Node (TreeElement (Nothing) (Just ActionLeft)) [],
      DT.Node (TreeElement (Just P2) (Just ActionRight)) [
          DT.Node (TreeElement (Nothing) (Just Heads)) [],
          DT.Node (TreeElement (Nothing) (Just Forfeit)) [],
          DT.Node (TreeElement (Nothing) (Just Tails)) []
                                                         ]
                                                   ],
  DT.Node (TreeElement (Just P1)     (Just Tails)) [
      DT.Node (TreeElement (Nothing) (Just ActionLeft)) [],
      DT.Node (TreeElement (Just P2) (Just ActionRight)) [
          DT.Node (TreeElement (Nothing) (Just Heads)) [],
          DT.Node (TreeElement (Nothing) (Just Forfeit)) [],
          DT.Node (TreeElement (Nothing) (Just Tails)) []
                                                         ]
                                                   ]
                                              ]

--fn = CMS.runState (getInfoMap gt) ([],DM.empty)
--infoMap = snd $ snd fn
--pfn = putStrLn $ showTree $ infoMap

actionsFromHeads = (_A gt [Heads])
playerFromHeads = (_P gt [Heads])
playerFromHeadsHeads = (_P gt [Heads,Heads])

actionsFromHeads' = (_A' gt [Just Heads])
playerFromHeads' = (_P' gt [Just Heads])
playerFromHeadsHeads' = (_P' gt [Just Heads,Just Heads])

_Z :: (Eq action, Ord action) => (InformationMap action) -> Maybe (DS.Set (History action))
_Z = DM.lookup DS.empty 

--h = [Tails,ActionRight] :: (History Action)

--maybeInfoSet = _I gt infoMap h

--fn' = CMS.runState (getInfoMapsState gt) ([],DM.empty)
--infoMaps' = snd $ snd fn'
--pfn' = putStrLn $ showTree $ infoMaps'

--p1InfoMap = DM.lookup (Just P1) infoMaps'
--p2InfoMap = DM.lookup (Just P2) infoMaps'
--chanceInfoMap = DM.lookup (Just Chance) infoMaps'
--nothingInfoMap = DM.lookup Nothing infoMaps'

--infoMaps'' = getInfoMapsState gt

infoMaps' = getInfoMaps gt

p1InfoMap = DM.lookup (Just P1) infoMaps'
p2InfoMap = DM.lookup (Just P2) infoMaps'
chanceInfoMap = DM.lookup (Just Chance) infoMaps'
nothingInfoMap = DM.lookup Nothing infoMaps'
