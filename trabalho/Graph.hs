module Graph where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Control.Monad.State

type NodeId = Int
type PortId = Int

data Link = Link NodeId PortId
    deriving (Show, Eq)

data AgentType = Cons | Nil | Append | Succ | Zero | Add | Duplicator | Eraser
    deriving (Show, Eq)

data Node = Node {
    agent :: AgentType,
    ports :: IntMap Link
} deriving (Show)

data Graph = Graph {
    nodes :: IntMap Node,
    activePairs :: [(NodeId, NodeId)],
    nextId :: NodeId             
} deriving (Show)

type NetState a = State Graph a

-- ar(\alpha) + 1 (porta principal)
arity :: AgentType -> Int
arity Cons          = 3
arity Nil           = 1
arity Append        = 3
arity Succ          = 2
arity Zero          = 1
arity Add           = 3
arity Duplicator    = 3
arity Eraser        = 1

emptyGraph :: Graph
emptyGraph = Graph {
    nodes = Map.empty,
    activePairs = [],
    nextId = 0
}

lookupNode :: NodeId -> Graph -> Maybe Node
lookupNode n = Map.lookup n . nodes

addNode :: AgentType -> NetState NodeId
addNode agentType = do
    g <- get
    let nodeId = (nextId g) + 1
        newNode = Node { agent = agentType, ports = Map.empty }
        nodes' = Map.insert nodeId newNode (nodes g)
    put g { nodes = nodes', nextId = nodeId }
    return nodeId

lookupPort :: NodeId -> PortId -> Graph -> Maybe Link
lookupPort n p g = do
    node <- Map.lookup n (nodes g)
    Map.lookup p (ports node)

deleteNode :: NodeId -> NetState ()
deleteNode n = modify $ \g -> g {
    nodes = Map.delete n (nodes g)
}

connectPort :: NodeId -> PortId -> NodeId -> PortId -> NetState ()
connectPort from fromPort to toPort =
    updatePorts from $
    Map.insert fromPort (Link to toPort)

unlinkPorts :: NodeId -> [PortId] -> NetState ()
unlinkPorts _ [] = pure ()
unlinkPorts nodeId (p:ps) = do
    unlink (nodeId, p)
    unlinkPorts nodeId ps

updatePorts :: NodeId -> (IntMap Link -> IntMap Link) -> NetState ()
updatePorts n f = modify $ \g -> g {
    nodes = Map.adjust (\node -> node {ports = f (ports node)}) n (nodes g)
}

enqueueActivePair :: NodeId -> NodeId -> NetState ()
enqueueActivePair n1 n2 = modify $ \g -> g {
    activePairs = (n1, n2) : activePairs g
}

link :: (NodeId, PortId) -> (NodeId, PortId) -> NetState ()
link (n1, p1) (n2, p2) = do
    unlink (n1, p1)
    unlink (n2, p2)

    connectPort n1 p1 n2 p2
    connectPort n2 p2 n1 p1

    if p1 == 0 && p2 == 0
        then enqueueActivePair n1 n2
        else return ()

unlink :: (NodeId, PortId) -> NetState ()
unlink (n, p) = do
    g <- get
    case lookupPort n p g of
        Nothing -> pure ()
        Just (Link otherNode otherPort) -> do
            updatePorts n         (Map.delete p)
            updatePorts otherNode (Map.delete otherPort)

removeNode :: NodeId -> NetState ()
removeNode nodeId = do
    g <- get
    case lookupNode nodeId g of
        Nothing   -> pure ()
        Just node -> do
            unlinkPorts nodeId (Map.keys $ ports node)
            deleteNode nodeId
