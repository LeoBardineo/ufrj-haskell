module Utils where

import Graph

import qualified Data.IntMap.Strict as Map
import Control.Monad.State

formatLink :: Link -> String
formatLink (Link targetId targetPort) = "Nó #" ++ show targetId ++ " (Porta " ++ show targetPort ++ ")"

formatPort :: (PortId, Link) -> String
formatPort (portId, link) = "    Porta " ++ show portId ++ " -> " ++ formatLink link ++ "\n"

formatPorts :: Node -> String
formatPorts node= if Map.null (ports node)
    then "    (Nenhuma porta conectada)\n"
    else concat $ map formatPort (Map.toList (ports node))

formatNode :: NodeId -> Node -> String
formatNode nodeId node = "Nó #" ++ show nodeId ++ " [" ++ show (agent node) ++ "]:\n" ++
    formatPorts node

formatNodes :: Graph -> String
formatNodes g = if Map.null (nodes g)
    then "Grafo Vazio\n"
    else concat $ map (\(nodeId, node) -> formatNode nodeId node) (Map.toList (nodes g))

formatGraph :: Graph -> String
formatGraph g = formatNodes g ++
  "Pares Ativos (Cortes): " ++ show (activePairs g) ++ "\n" ++
  "Próximo ID de Nó:     " ++ show (nextId g) ++ "\n"

printGraphs :: [Graph] -> IO ()
printGraphs [] = return ()
printGraphs (g:gs) = do
    putStrLn $ "----- #" ++ show (length gs) ++ " -----"
    putStrLn (formatGraph g)
    printGraphs gs
