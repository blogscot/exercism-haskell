module LinkedList where

import qualified Data.Foldable as F

data LinkedList a = Node a (LinkedList a) | Nil deriving (Show, Eq, Ord)

instance F.Foldable LinkedList where
  foldr _ acc Nil = acc
  foldr f acc (Node k linked) = f k (F.foldr f acc linked)

nil :: LinkedList a
nil = Nil

isNil :: LinkedList a -> Bool
isNil linked = case linked of
                Nil -> True
                _ -> False

toList :: LinkedList a -> [a]
toList linked = case linked of
                  Node x next' -> x : toList next'
                  _ -> []

fromList :: [a] -> LinkedList a
fromList = foldr new Nil

datum :: LinkedList a -> a
datum linked = case linked of
                Node x _ -> x
                _ -> error "Invalid parameter."

next :: LinkedList a -> LinkedList a
next linked = case linked of
               Node _ next' -> next'
               _ -> error "Unexpected end of list."

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = F.foldl (flip new) Nil

new :: a -> LinkedList a -> LinkedList a
new = Node

main =
  print $ F.foldr (+) 0 $ fromList [2..10]

