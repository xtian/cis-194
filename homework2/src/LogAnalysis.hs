module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage line =
  case line of
    ('I':s) -> buildMessage Info s
    ('W':s) -> buildMessage Warning s
    ('E':s) -> buildMessage (Error $ read $ head parts) (unwords $ tail parts)
                 where parts = words s
    _       -> Unknown line
  where
    buildMessage messageType s =
      let (timestamp:message) = words s
      in  LogMessage messageType (read timestamp) (unwords message)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree

insert _ (Node _ (Unknown _) _) = error "MessageTree contains Unknown"

insert message Leaf = Node Leaf message Leaf

insert msg@(LogMessage _ t1 _) (Node left nMsg@(LogMessage _ t2 _) right) =
  if t1 <= t2
    then Node (insert msg left) nMsg right
    else Node left nMsg (insert msg right)


build :: [LogMessage] -> MessageTree
build messages = foldl fn Leaf messages
  where fn tree message = insert message tree
