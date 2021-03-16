module GameFunctions
    (
    gameTraverse,
    drawGameTree,
    putStrGameTree,
    putStrMaybeGameTree,
    getInformationSets,
    insertInfoSet,
    getActions,
    sameInfoSet
    ) where
import GameTypes
import qualified Data.Map as DM
import qualified Data.Set as DS 

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

getInformationSets :: GameTree -> InformationSets
getInformationSets tree = traverseHelp DM.empty (GameTypes.subForest tree) []
  where
  traverseHelp infoSet forest actions = Prelude.foldl (moreHelp actions) (insertInfoSet forest actions infoSet) forest
  moreHelp actions infoSet (action,Nothing) = DM.insertWith DS.union DS.empty (DS.singleton (actions++[action])) infoSet
  moreHelp actions infoSet (action, Just tree) = traverseHelp infoSet (GameTypes.subForest tree) (actions++[action])

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
  helper (Just t1) (Just t2) = (getActions (GameTypes.subForest t1)) == (getActions (GameTypes.subForest t2))
