module GameExample
  (
  )where

import GameTypes
import GameFunctions

--data Player = P1 | P2 | Chance deriving (Eq,Show) --Int


data ExampleAction = ExHeads | ExTails | ExActionLeft | ExActionRight | ExForfeit deriving (Eq,Ord,Show)

instance MyAction ExampleAction where
  specificActions = [ExHeads,ExTails,ExActionLeft,ExActionRight,ExForfeit]

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
  ] :: GameTree
