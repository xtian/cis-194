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
