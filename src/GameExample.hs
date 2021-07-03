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
f = CMS.runState (buildInfoMap gg) ([],DM.empty)
pf = putStrLn $ showTree $ snd $ snd f

actionsFromHeads = (_A gg [Heads])
playerFromHeads = (_P gg [Heads])
playerFromHeadsHeads = (_P gg [Heads,Heads])

{-
playerFromHeads = (_P gg [Heads])
actionFromInfoSet = anInfoSetOfG >>= (_AofI g)
-}
{-
g = GameNode Chance
  [
    (Heads, Just (GameNode P1
                  [(ActionLeft, Nothing),
                   (ActionRight, Just (GameNode P2
                                       [(Heads, Nothing),
                                        (Forfeit, Nothing),
                                        (Tails, Nothing)
                                       ]
                                      )
                   )
                  ]
                 )
    ),
    (Tails, Just (GameNode P1
                  [(ActionLeft, Nothing),
                   (ActionRight, Just (GameNode P2
                                       [(Heads, Nothing),
                                        (Forfeit, Nothing),
                                        (Tails, Nothing)
                                       ]
                                      )
                   )
                  ]
                 )
    )
  ] 

infoMapOfG = getInformationMap g
infoSetsOfG = getSetOfInfoSets infoMapOfG
historyOfG = _H infoMapOfG
terminalHistoryOfG = _Z g
anInfoSetOfG = _I infoSetsOfG [Heads,ActionRight]
aFilteredInfoSetOfP1 = filterInfoSetByPlayer g P1 (pureSet anInfoSetOfG)
aFilteredInfoSetOfP2 = filterInfoSetByPlayer g P2 (pureSet anInfoSetOfG)


sigma :: (Show player, Show action, Ord action, Eq action) =>
  (GameTree player action) -> Sigma action
sigma g = \h -> \a -> helper (gameTraverse g h) a
  where
  helper Nothing _ = 0 :: Float
  helper (Just subTree) a = helper2 (getActions (subForest subTree)) a
  helper2 actionSet a = if DS.member a actionSet then (fromIntegral 1) / (fromIntegral (DS.size actionSet))
                        else 0 :: Float
n1 = DT.Node 5 []
n2 = DT.Node 4 [n1]

gameStateIntance = GameState [] g
--}
