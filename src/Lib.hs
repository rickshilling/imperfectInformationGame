{-# LANGUAGE FlexibleContexts #-}

module Lib
    (
    ) where

import Data.Set
import Data.Tree
import Data.Graph
import Control.Monad
import Data.Map
import qualified Data.Map as DM
import qualified Data.Set as DS
--import qualified Prelude as P

data Player = P1 | P2 | Chance deriving (Eq,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit | A | B | C | D deriving (Eq,Ord,Show)
type Payout = Float
type P = Set Player
type Probability = Float
data GameTree = GameNode {
        rootLabel :: Player,
        subForest :: [(Action, Maybe GameTree)]
}

gameTraverse :: GameTree -> [Action] -> Maybe GameTree
gameTraverse t [] = Just t
gameTraverse (GameNode p []) (refA:as) = Nothing
gameTraverse (GameNode p ((currA, Nothing):rForests)) (refA:as) =
  if currA == refA then Nothing
  else gameTraverse (GameNode p rForests) (refA:as)
gameTraverse (GameNode p ((currA,Just mTree):rForests)) (refA:as) =
  if currA == refA then gameTraverse mTree as
  else gameTraverse (GameNode p rForests) (refA:as)

drawGameTree :: GameTree -> [String]
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

putStrGameTree :: GameTree -> IO ()
putStrGameTree g = putStr $ (unlines . drawGameTree) g

putStrMaybeGameTree :: Maybe GameTree -> IO ()
putStrMaybeGameTree Nothing = putStrLn "Nothing"
putStrMaybeGameTree (Just gt) = putStrGameTree gt

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

getHistoryfromElement :: (Action, Maybe GameTree) -> Set [Action]
getHistoryfromElement (currentAction, Nothing) = DS.fromList [[currentAction]]
getHistoryfromElement (currentAction, Just gameTree) = DS.union (DS.map ([currentAction] ++ ) subSets) (DS.fromList [[currentAction]])
  where
    subSets = Prelude.foldl DS.union DS.empty (Prelude.map getHistoryfromElement (Lib.subForest gameTree))

getHistories :: GameTree -> Set [Action]
getHistories (GameNode _ []) = DS.empty
getHistories (GameNode _ forest) = Prelude.foldl DS.union DS.empty (Prelude.map getHistoryfromElement forest)

getTerminalHistoryfromElement :: (Action, Maybe GameTree) -> Set [Action]
getTerminalHistoryfromElement (currentAction, Nothing) = Data.Set.fromList [[currentAction]]
getTerminalHistoryfromElement (currentAction, Just gameTree) = Data.Set.map ([currentAction] ++ ) subSets
  where
    subSets = Prelude.foldl DS.union DS.empty (Prelude.map getTerminalHistoryfromElement (Lib.subForest gameTree)) 

getTerminalHistories :: GameTree -> Set [Action]
getTerminalHistories (GameNode _ []) = Data.Set.empty
getTerminalHistories (GameNode _ forest) = Prelude.foldl Data.Set.union Data.Set.empty (Prelude.map getTerminalHistoryfromElement forest)

_H = getHistories g

_Z = getTerminalHistories g

getActionSet :: GameTree -> [Action] -> Set Action
getActionSet gameTree actions = (helper . gameTraverse gameTree) actions
  where
    helper Nothing = DS.empty
    helper (Just gameTree) = DS.fromList $ Prelude.map fst (Lib.subForest gameTree)

_P :: [Action] -> Maybe Player
_P h = (helper . gameTraverse g) h
  where
    helper Nothing = Nothing
    helper (Just myGameNode) = Just (Lib.rootLabel myGameNode)
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
type InformationSets = Map (Set Action) (Set [Action])
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

getInformationSets :: GameTree -> InformationSets
getInformationSets tree = traverseHelp DM.empty (Lib.subForest tree) []
  where
  traverseHelp infoSet forest actions = Prelude.foldl (moreHelp actions) (insertInfoSet forest actions infoSet) forest
  moreHelp actions infoSet (action,Nothing) = DM.insertWith DS.union DS.empty (DS.singleton (actions++[action])) infoSet
  moreHelp actions infoSet (action, Just tree) = traverseHelp infoSet (Lib.subForest tree) (actions++[action])

insertInfoSet :: [(Action, Maybe GameTree)] -> [Action] -> InformationSets -> InformationSets
insertInfoSet forest actions infoSet = DM.insertWith DS.union (getActions forest) (DS.singleton actions) infoSet

getActions :: [(Action, Maybe GameTree)] -> DS.Set Action
getActions = Prelude.foldl (\set -> \element -> DS.union set (DS.singleton (fst element))) DS.empty

sameInfoSet :: GameTree -> [Action] -> [Action] -> Bool
sameInfoSet tree a1 a2 = helper (gameTraverse tree a1) (gameTraverse tree a2)
  where
  helper Nothing   Nothing   = True
  helper Nothing   _         = False
  helper _         Nothing   = False
  helper (Just t1) (Just t2) = (getActions (Lib.subForest t1)) == (getActions (Lib.subForest t2))

populateInformationSets :: GameTree -> InformationSets
populateInformationSets gameTree = DS.foldl helper DM.empty (getHistories gameTree)
  where
    helper infoSets actionList = DM.insertWith Data.Set.union (getActionSet gameTree actionList) (DS.fromList [actionList]) infoSets

gg = GameNode P1 [(B,Just (GameNode P2 [(B,Nothing),(C,Nothing),(D,Nothing)])),(C,Nothing),(D,Nothing)]

sndHelper :: GameTree         -> [Action] -> InformationSets -> InformationSets
sndHelper    (GameNode _ forest) actionList  currentInfo      =
  DM.insertWith Data.Set.union (DS.fromList $ Prelude.map fst forest) (DS.fromList [actionList]) currentInfo

ir = sndHelper gg [] DM.empty

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
data GameState = GameVariables {
  myGameTree :: GameTree,
  u :: DM.Map Int ((Set [Action]) -> Float),
  _A :: [Action] -> Set Action
}
  --_I :: 

gs = GameVariables g DM.empty (getActionSet g)

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
    helper (Just gameTree) = fromList $ Prelude.map fst (Lib.subForest gameTree)
-}
