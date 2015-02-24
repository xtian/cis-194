module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage line =
  case line of
    ('I':s) -> buildMessage Info s
    ('W':s) -> buildMessage Warning s
    ('E':s) -> buildMessage (Error $ read severity) (unwords message)
                 where (severity:message) = words s
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
build messages = foldl (flip insert) Leaf messages


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map toStr $ filter important $ inOrder $ build messages
  where
    important (LogMessage (Error i) _ _) = i > 50
    important _ = False
    toStr (LogMessage _ _ message) = message
    toStr _ = error "Unknown messages not allowed."
