module Main where
import XmlTemplate.Head
import XmlTemplate.Monad
import XmlTemplate.Imgs
import Library.System

main = catchExceptions $ do
  as <- getArgs
  mapM_ processHeadFile as