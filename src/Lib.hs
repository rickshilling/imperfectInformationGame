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
ofTheSameInfoSet :: [Action] -> [Action] -> Bool
ofTheSameInfoSet h h' = (_A h) == (_A h')
informationSets :: [(Set Action,Set [Action])]
informationSets = Data.Set.foldl helper [] _H
  where
    helper []                                         currentActionSequence = [(_A currentActionSequence, fromList [currentActionSequence])]
    helper ((actionSet, infoSet):remainingInfoSets)   currentActionSequence =
      if actionSet == (_A currentActionSequence) then [(actionSet, (Data.Set.insert currentActionSequence infoSet) )] ++ remainingInfoSets
      else [(actionSet, infoSet)] ++ helper remainingInfoSets currentActionSequence

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

-}
{-
populateInformationSets :: Set [Action] -> InformationSets
populateInformationSets = Data.Set.foldl helper DM.empty 
  where
    helper infoSets actionList = DM.insertWith Data.Set.union (_A actionList) (fromList [actionList]) infoSets
-}
{-
data GameTree = GameNode {
        rootLabel :: Player,
        subForest :: [(Action, Maybe GameTree)]
}
-}


{-
populateInformationSets :: GameTree -> InformationSets
populateInformationSets gameTree = DS.foldl helper DM.empty (getHistories gameTree)
  where
    helper infoSets actionList = DM.insertWith Data.Set.union (getActionSet gameTree actionList) (DS.fromList [actionList]) infoSets
-}
{-
gg = GameNode P1 [(B,Just (GameNode P2 [(B,Nothing),(C,Nothing),(D,Nothing)])),(C,Nothing),(D,Nothing)]

sndHelper :: GameTree         -> [Action] -> InformationSets -> InformationSets
sndHelper    (GameNode _ forest) actionList  currentInfo      =
  DM.insertWith Data.Set.union (DS.fromList $ Prelude.map fst forest) (DS.fromList [actionList]) currentInfo

ir = sndHelper gg [] DM.empty
-}
{-
is :: InformationSets
is = DM.insert (fromList [B,C,D]) (fromList [[A]]) ir
-}

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
data TTree = TEmpty | TLeaf Int | TNode (TTree) Int (TTree)
instance Foldable TTree where
  foldMap f TEmpty = mempty
  foldMap f (TLeaf x) = f x
  foldMap f (TNode l x r) = foldMap f l `mappend` f x `mappend` foldMap f r
-}
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

--
--
-- March 6 2021
--
--
{-_A :: [Action] -> Set Action
_A h = (helper . gameTraverse g) h
  where
    helper Nothing = empty
    helper (Just gameTree) = fromList $ Prelude.map fst (GameTypes.subForest gameTree)
-}
