module LinkedList where

data LinkedList a = Node a (LinkedList a) | Nil deriving (Show, Eq, Ord)

nil :: LinkedList a
nil = Nil

isNil :: LinkedList a -> Bool
isNil linked = case linked of
                Nil -> True
                _ -> False

toList :: LinkedList a -> [a]
toList linked = case linked of
                  Node x next -> x : toList next
                  _ -> []

fromList :: [a] -> LinkedList a
fromList = foldr new Nil

datum :: LinkedList a -> a
datum linked = case linked of
                Node x _ -> x
                _ -> error "Invalid parameter."

next :: LinkedList a -> LinkedList a
next linked = case linked of
               Node _ next -> next
               _ -> error "Unexpected end of list."

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linked = fromList . reverse $ toList linked

new :: a -> LinkedList a -> LinkedList a
new = Node

one = new (1 :: Int) nil
two = new (2 :: Int) one
three = new (3 :: Int) two

main = do
  print three
  print $ datum three
  print $ next three
  print $ toList three
  print $ fromList [1..5]
