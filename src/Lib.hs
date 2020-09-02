module Lib
    (
    ) where

import Data.Set
import Data.Tree
import Data.Graph
import Control.Monad

data Player = P1 | P2 | Chance deriving (Eq,Show)
data Action = Heads | Tails | ActionLeft | ActionRight | Forfeit deriving (Eq,Ord,Show)
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
gameTraverse (GameNode p ((currA,   Nothing):rForests)) (refA:as) =
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
getHistoryfromElement (currentAction, Nothing) = fromList [[currentAction]]
getHistoryfromElement (currentAction, Just gameTree) = union (Data.Set.map ([currentAction] ++ ) subSets) (fromList [[currentAction]])
  where
    subSets = Prelude.foldl union empty (Prelude.map getHistoryfromElement (Lib.subForest gameTree))

getHistories :: GameTree -> Set [Action]
getHistories (GameNode _ []) = empty
getHistories (GameNode _ forest) = Prelude.foldl union empty (Prelude.map getHistoryfromElement forest)

getTerminalHistoryfromElement :: (Action, Maybe GameTree) -> Set [Action]
getTerminalHistoryfromElement (currentAction, Nothing) = fromList [[currentAction]]
getTerminalHistoryfromElement (currentAction, Just gameTree) = Data.Set.map ([currentAction] ++ ) subSets
  where
    subSets = Prelude.foldl union empty (Prelude.map getTerminalHistoryfromElement (Lib.subForest gameTree)) 

getTerminalHistories :: GameTree -> Set [Action]
getTerminalHistories (GameNode _ []) = empty
getTerminalHistories (GameNode _ forest) = Prelude.foldl union empty (Prelude.map getTerminalHistoryfromElement forest)

_H = getHistories g

_Z = getTerminalHistories g

_A :: [Action] -> Set Action
_A h = (helper . gameTraverse g) h
  where
    helper Nothing = empty
    helper (Just gameTree) = fromList $ Prelude.map fst (Lib.subForest gameTree)

_P :: [Action] -> Maybe Player
_P h = (helper . gameTraverse g) h
  where
    helper Nothing = Nothing
    helper (Just myGameNode) = Just (Lib.rootLabel myGameNode)

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


