{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module PeopleDB where
import Library.System 
import System.Directory hiding (getDirectoryContents)
import Text.JSON.Generic
import System.IO.Unsafe
import Library
import PlainTemplate.Monad

data Person = Person
  { name :: String
  , fac :: String
  , articles :: [ FilePath ] }
  deriving (Show, Data, Typeable)

savePerson :: (MonadIO m) => Int -> Person -> m ()
savePerson n p = liftIO $ do
  let d = "x:/~db/people/" ++ show n ++ "/"
  createDirectory d
  writeFileE utf8 (d ++ "person.json") $ encode $ ptoJ p 
  writeFile (d ++ "articles") $ intercalate "\n" $ articles p
  where
    ptoJ Person{..} = makeObj
      [ ("name", showJSON name)
      , ("fac", showJSON fac) ]

listPeople :: MonadIO m => m [FilePath]
listPeople = liftIO $ getDirectoryContents "x:/~db/people/"

readPerson :: MonadIO m => [Char] -> m Person
readPerson d = liftIO $ do
  Ok o <- decode `liftM` readFileE utf8 (d ++ "/person.json")
  articles <- lines `liftM` readFile (d ++ "/articles")
  let Ok name = valFromObj "name" o
      Ok fac = valFromObj "fac" o
  return Person{..}

listArticles :: MonadIO m => [Char] -> m [FilePath]
listArticles path = do
  Person{..} <- readPerson path
  return articles
  
fullName :: (MonadIO m) => [Char] -> m String
fullName path = do
  Person{..} <- readPerson path
  return $ printf "%s %s" name fac

findPersonByFN :: MonadIO m => String -> m (Maybe String)
findPersonByFN fn = do
  ppl <- listPeople
  findM (\ p -> liftM (fn ==) $ fullName p) ppl

newIndex :: MonadIO m => m Int
newIndex = do
  x <- liftIO $ getDirectoryContentsL "x:/~db/people/"
  return $ 1 + maximum (map read x)

-----------
-- FIXME --
-----------
personFiles fp = map (drop 2 fp ++) ["/person.json", "/articles"]
