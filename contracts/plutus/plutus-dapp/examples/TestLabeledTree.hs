module Main where

{-- FIXME }
import Data.ByteString.UTF8
import IntegriTree.Tree
import System.IO ()

main :: IO ()
main = do
  -- Create a new tree with some childrem
  let node = Node (fromString "`", fromString "alma") (Node (fromString "alma", fromString "barack") (Node (fromString "barack", fromString "korte") leaf leaf) leaf) (Node (fromString "korte", fromString "{") leaf leaf)
  let nodeU = Node (fromString "`", fromString "alma") (Node (fromString "alma", fromString "barack") (Node (fromString "barack", fromString "korte") leaf leaf) (Node (fromString "szilva", fromString "{") leaf leaf)) (Node (fromString "korte", fromString "szilva") leaf leaf)

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
-}
