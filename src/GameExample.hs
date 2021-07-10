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

data Player = P1 | P2 | Chance deriving (Eq,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show)

gg =
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

fn = CMS.runState (getInfoMap gg) ([],DM.empty)
pfn = putStrLn $ showTree $ snd $ snd fn

actionsFromHeads = (_A gg [Heads])
playerFromHeads = (_P gg [Heads])
playerFromHeadsHeads = (_P gg [Heads,Heads])
