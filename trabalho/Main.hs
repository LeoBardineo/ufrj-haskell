module Main where

import Graph
import Utils
import Evaluator

import Control.Monad.State

testeComputacao :: NetState [Graph]
testeComputacao = do
  s1 <- addNode Succ
  z1 <- addNode Zero
  link (s1, 1) (z1, 0)
  
  dup <- addNode Duplicator
  link (dup, 0) (s1, 0)

  evalNet

testeCons :: NetState [Graph]
testeCons = do
  c1 <- addNode Cons
  a1 <- addNode Append
  link (c1, 1) (a1, 0)

  evalNet

main :: IO ()
main = do
  putStrLn "Avaliando Interaction Net..."
  
  let (graphs, grafoFinal) = runState testeCons emptyGraph
  printGraphs graphs

  putStrLn "\n--- Grafo final ---"
  putStrLn (formatGraph grafoFinal)
