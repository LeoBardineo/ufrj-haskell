module Evaluator where

import Graph
import Rules

import Control.Monad.State
import qualified Data.IntMap.Strict as Map

reducePair :: (NodeId, NodeId) -> NetState ()
reducePair (id1, id2) = do
    g <- get
    let mNode1 = Map.lookup id1 (nodes g)
        mNode2 = Map.lookup id2 (nodes g)
    case (mNode1, mNode2) of
        (Just node1, Just node2) -> applyRule id1 node1 id2 node2
        _ -> return ()

evalNet :: NetState [Graph]
evalNet = do
    g <- get
    case activePairs g of
        [] -> return [g]
        ((n1,n2):rest) -> do
            put g { activePairs = rest }
            reducePair (n1,n2)
            gs <- evalNet
            return (g : gs)
