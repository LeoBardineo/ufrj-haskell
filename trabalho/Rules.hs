module Rules where

import Graph
import Utils
import Helpers

import Control.Monad.State
import qualified Data.IntMap.Strict as Map

applyRule :: NodeId -> Node -> NodeId -> Node -> NetState ()
applyRule idA nodeA idB nodeB = case (agent nodeA, agent nodeB) of
    (Add, Zero) -> ruleAddZero idA idB
    (Zero, Add) -> ruleAddZero idB idA
    (Add, Succ) -> ruleAddSucc idA idB
    (Succ, Add) -> ruleAddSucc idB idA

    (Append, Nil)  -> ruleAppendNil idA idB
    (Nil, Append)  -> ruleAppendNil idB idA
    (Append, Cons) -> ruleAppendCons idA idB
    (Cons, Append) -> ruleAppendCons idB idA

    (Eraser, Zero) -> ruleEraserBase idA idB
    (Zero, Eraser) -> ruleEraserBase idB idA
    (Eraser, Nil)  -> ruleEraserBase idA idB
    (Nil, Eraser)  -> ruleEraserBase idB idA

    (Eraser, Succ) -> ruleEraserSucc idA idB
    (Succ, Eraser) -> ruleEraserSucc idB idA

    (Duplicator, Zero) -> ruleDuplicatorZero idA idB
    (Zero, Duplicator) -> ruleDuplicatorZero idB idA

    (Duplicator, Succ) -> ruleDuplicatorSucc idA idB
    (Succ, Duplicator) -> ruleDuplicatorSucc idB idA

    _ -> error $ "Colisão não implementada entre: " ++ show (agent nodeA) ++ " e " ++ show (agent nodeB)

ruleAddZero :: NodeId -> NodeId -> NetState ()
ruleAddZero idAdd idZero = do
    linkY <- getLink idAdd 1
    linkOut <- getLink idAdd 2
    removeNode idAdd
    removeNode idZero
    connectLinks linkY linkOut

ruleAddSucc :: NodeId -> NodeId -> NetState ()
ruleAddSucc idAdd idSucc = do
    linkY <- getLink idAdd 1
    linkOut <- getLink idAdd 2
    linkPred <- getLink idSucc 1
    removeNode idAdd
    removeNode idSucc
    newAdd  <- addNode Add
    newSucc <- addNode Succ
    linkTo (newAdd, 0) linkPred
    linkTo (newAdd, 1) linkY
    link (newSucc, 1) (newAdd, 2)
    linkTo (newSucc, 0) linkOut

ruleAppendNil :: NodeId -> NodeId -> NetState ()
ruleAppendNil idAppend idNil = do
    linkY <- getLink idAppend 1
    linkOut <- getLink idAppend 2
    removeNode idAppend
    removeNode idNil
    connectLinks linkY linkOut

ruleAppendCons :: NodeId -> NodeId -> NetState ()
ruleAppendCons idAppend idCons = do
    linkY <- getLink idAppend 1
    linkOut <- getLink idAppend 2
    linkHead <- getLink idCons 1
    linkTail <- getLink idCons 2
    removeNode idAppend
    removeNode idCons
    newAppend <- addNode Append
    newCons <- addNode Cons
    linkTo (newAppend, 0) linkTail
    linkTo (newAppend, 1) linkY
    linkTo (newCons, 1) linkHead
    link (newCons, 2) (newAppend, 2)
    linkTo (newCons, 0) linkOut

ruleEraserBase :: NodeId -> NodeId -> NetState ()
ruleEraserBase idEraser idBase = do
    removeNode idEraser
    removeNode idBase

ruleEraserSucc :: NodeId -> NodeId -> NetState ()
ruleEraserSucc idEraser idSucc = do
    linkPred <- getLink idSucc 1
    removeNode idEraser
    removeNode idSucc
    newEraser <- addNode Eraser
    linkTo (newEraser, 0) linkPred

ruleDuplicatorZero :: NodeId -> NodeId -> NetState ()
ruleDuplicatorZero idDup idZero = do
    linkOut1 <- getLink idDup 1
    linkOut2 <- getLink idDup 2
    removeNode idDup
    removeNode idZero
    z1 <- addNode Zero
    z2 <- addNode Zero
    linkTo (z1, 0) linkOut1
    linkTo (z2, 0) linkOut2

ruleDuplicatorSucc :: NodeId -> NodeId -> NetState ()
ruleDuplicatorSucc idDup idSucc = do
    linkOut1 <- getLink idDup 1
    linkOut2 <- getLink idDup 2
    linkPred <- getLink idSucc 1
    removeNode idDup
    removeNode idSucc
    s1 <- addNode Succ
    s2 <- addNode Succ
    newDup <- addNode Duplicator
    linkTo (newDup, 0) linkPred
    link (newDup, 1) (s1, 1)
    link (newDup, 2) (s2, 1)
    linkTo (s1, 0) linkOut1
    linkTo (s2, 0) linkOut2
