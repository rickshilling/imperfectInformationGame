{-# LANGUAGE FlexibleContexts #-}

module Lib
    (
    ) where
import Data.Tree
import Data.Graph
import Control.Monad
import Data.Map
import qualified Data.Map as DM
import qualified Data.Set as DS
import GameTypes
import GameFunctions
--import qualified Prelude as P

{-
getHistoryfromElement :: (Action, Maybe GameTree) -> Set [Action]
getHistoryfromElement (currentAction, Nothing) = DS.fromList [[currentAction]]
getHistoryfromElement (currentAction, Just gameTree) = DS.union (DS.map ([currentAction] ++ ) subSets) (DS.fromList [[currentAction]])
  where
    subSets = Prelude.foldl DS.union DS.empty (Prelude.map getHistoryfromElement (GameTypes.subForest gameTree))

getHistories :: GameTree -> Set [Action]
getHistories (GameNode _ []) = DS.empty
getHistories (GameNode _ forest) = Prelude.foldl DS.union DS.empty (Prelude.map getHistoryfromElement forest)

getTerminalHistoryfromElement :: (Action, Maybe GameTree) -> Set [Action]
getTerminalHistoryfromElement (currentAction, Nothing) = Data.Set.fromList [[currentAction]]
getTerminalHistoryfromElement (currentAction, Just gameTree) = Data.Set.map ([currentAction] ++ ) subSets
  where
    subSets = Prelude.foldl DS.union DS.empty (Prelude.map getTerminalHistoryfromElement (GameTypes.subForest gameTree)) 

getTerminalHistories :: GameTree -> Set [Action]
getTerminalHistories (GameNode _ []) = Data.Set.empty
getTerminalHistories (GameNode _ forest) = Prelude.foldl Data.Set.union Data.Set.empty (Prelude.map getTerminalHistoryfromElement forest)

_H = getHistories g

_Z = getTerminalHistories g

getActionSet :: GameTree -> [Action] -> Set Action
getActionSet gameTree actions = (helper . gameTraverse gameTree) actions
  where
    helper Nothing = DS.empty
    helper (Just gameTree) = DS.fromList $ Prelude.map fst (GameTypes.subForest gameTree)

_P :: [Action] -> Maybe Player
_P h = (helper . gameTraverse g) h
  where
    helper Nothing = Nothing
    helper (Just myGameNode) = Just (GameTypes.rootLabel myGameNode)
--}
{-

_I :: [Action] -> Set [Action]
_I h = helper informationSets h
  where
    helper [] _ = empty
    helper ((actionSet, infoSet):remainingInfoSets) h = if Data.Set.member h infoSet then infoSet else helper remainingInfoSets h

actionsFromInformationSet :: (Set Action,Set [Action]) -> Set Action
actionsFromInformationSet = fst

__A = actionsFromInformationSet

sigma :: (Set Action,Set [Action]) -> DM.Map Action Float
sigma informationSet = undefined


{-
helper    (GameNode _ [])     _           currentInfo      = currentInfo
helper    (GameNode _ forest) actionList  currentInfo      = Prelude.foldl (subhelper actionList) currentInfo forest
  where
    --subhelper :: ([Action] -> InformationSets -> (Action, Maybe GameTree) -> InformationSets)
    subhelper       actionList  currentInfo        (currentAction, Nothing)  = DM.insertWith Data.Set.union (fromList $ Prelude.map fst forest) 
-}
{-
data GameState = GameVariables {
  myGameTree :: GameTree,
  u :: DM.Map Int ((Set [Action]) -> Float),
  _A :: [Action] -> Set Action
}
-}  --_I :: 

--gs = GameVariables g DM.empty (getActionSet g)

{-
class (Eq a) => MyPlayers a where
  players :: [a]
data MyPlayer = undefined
-}

newtype MyPlayer = MakePlayer Integer

toPlayer :: Integer -> MyPlayer
toPlayer x | x < 0 = error "Can't create negative player index"
           | otherwise = MakePlayer x

x = toPlayer 3

{-_A :: [Action] -> Set Action
_A h = (helper . gameTraverse g) h
  where
    helper Nothing = empty
    helper (Just gameTree) = fromList $ Prelude.map fst (GameTypes.subForest gameTree)
-}
