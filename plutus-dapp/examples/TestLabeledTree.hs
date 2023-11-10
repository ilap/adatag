module Main where

import Data.ByteString.UTF8
import Plutus.LabeledTree
import System.IO ()

main :: IO ()
main = do
  -- Create a new tree with some childrem
  let node = Node 1 (fromString "`", fromString "alma") (Node 2 (fromString "alma", fromString "barack") (Node 4 (fromString "barack", fromString "korte") leaf leaf) leaf) (Node 3 (fromString "korte", fromString "{") leaf leaf)
  let nodeU = Node 1 (fromString "`", fromString "alma") (Node 2 (fromString "alma", fromString "barack") (Node 4 (fromString "barack", fromString "korte") leaf leaf) (Node 5 (fromString "szilva", fromString "{") leaf leaf)) (Node 3 (fromString "korte", fromString "szilva") leaf leaf)

  putStrLn "############## U ###############"
  printTree node ""

  putStrLn "############## U'  (szilva added) ###############"
  printTree nodeU ""

  putStrLn "############## U' -> U ###############"
  let nodeU' = checkUpdate (fromString "szilva") (fromString "korte", fromString "{") (fromString "alma", fromString "barack") nodeU True
  printTree nodeU' ""

  putStrLn "############## Accumulator Check validate(U') == U ###############"
  putStr "Acc bef: "
  printProof node
  putStr "Acc aft: "
  printProof nodeU'
  print (proof node == proof nodeU')