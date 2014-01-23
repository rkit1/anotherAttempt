{-# LANGUAGE RecordWildCards, QuasiQuotes, NoMonomorphismRestriction, RecordWildCards #-}
module Pages.MainPage where

import Library
import Library.System
import PlainTemplate.Process
import PlainTemplate.Monad
import PlainTemplate.Parser
import PlainTemplate.Variable
import Head
import PlainTemplate.Listing
--import PlainTemplate.NeedRebuild
import qualified Data.Accessor.Monad.MTL.State as A
import qualified Data.Set as S
import PeopleDB

type Builder a = [a] -> [a]

data MainPage = MainPage
  { mpTitle :: String
  , mpLeft :: [Block]
  , mpRight :: [Block]
  , mpContent :: [String] }

data Block 
  = Raw String
  | RawFromFile FilePath
  | CommonFromFile FilePath
  | Common String String
    
    -- title list footer
  | Listing String FilePath String

blocksToString a = wide f a
  where 
    f (Raw a) = return a
    f (RawFromFile fp) = callRTPL fp
    f (CommonFromFile a) = do
      callRTPL a
      callRTPL "/~templates/widget.rtpl"      
    f (Common a b) = do
      "head" $= a
      "body" $= b
      callRTPL "/~templates/widget.rtpl"
    f (Listing h fp foot) = do
      "head" $= h
      ("listing" $=) =<< do
        items <- take 20 `liftM` readListing fp
        forM items $ \ i -> do
          str <- liftIO $ readWidgetHead i
          return $ mkVariables [ ("content", Variable str) ]
      "footer" $= foot
      ("body" $=) =<< callRTPL "/~templates/WidgetListing.rtpl"
      callRTPL "/~templates/widget.rtpl"

        

mkPage MainPage{..} = do
  "title" $= mpTitle
  "leftcolumn" $=. blocksToString mpLeft
  "rightcolumn" $=. blocksToString mpRight
  "news" $= map mkN mpContent
  callRTPL "/~templates/mainpage1.rtpl"
  where 
    mkN str = mkVariables [ ("content", Variable str) ]

runMP :: FilePath -> [FilePath] -> M MainPage -> IO ()
runMP fp list m = do
  x <- isNeedRebuild fp list
  case x of
    False -> printf "skipping %s\n" fp
    True -> do
      printf "processing %s ... " fp
      hFlush stdout
      r <- runM $ do
        a <- mkPage =<< m
        deps <- A.get depends
        liftIO $ do
          writeFileE utf8 fp a
          updateRebuildInfo fp list $ S.toList deps
      error . show <++> const (putStrLn "done") $ r


commonLeft = 
      [ CommonFromFile "/~templates/birthday.rtpl"
      , CommonFromFile "/~templates/search.rtpl"
      , RawFromFile "/~templates/banners.htm"
      , CommonFromFile "/~templates/contacts.rtpl"
      ]

mainRight = [Listing "&laquo;Я вспоминаю...&raquo;" "x:/~listings/remember" [sQ|<p><a href="/remember.htm">Все материалы</a><p>|]]

allPages = [ remember, songs, pishut, canadohodets, bayki, index, people, orlov, newVIIA, health ]

remember = page $ PageSpec 
  { pTitle = "&laquo;Я вспоминаю...&raquo;"
  , pListing = "x:/~listings/remember"
  , pPath = "x:/remember.htm"
  , pArchPath = "/archive/remember/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }


songs = page $ PageSpec 
  { pTitle = "Песни ВИИЯковцев"
  , pListing = "x:/~listings/songs"
  , pPath = "x:/songs.htm"
  , pArchPath = "/archive/songs/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

pishut = page $ PageSpec 
  { pTitle = "В Клуб Пишут"
  , pListing = "x:/~listings/pishut"
  , pPath = "x:/pishut.htm"
  , pArchPath = "/archive/pishut/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

canadohodets = page $ PageSpec 
  { pTitle = "Канадоходец З-96у"
  , pListing = "x:/~listings/canadohodets"
  , pPath = "x:/canadohodets.htm"
  , pArchPath = "/archive/canadohodets/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

bayki = page $ PageSpec 
  { pTitle = "Наши байки"
  , pListing = "x:/~listings/bayki"
  , pPath = "x:/bayki.htm"
  , pArchPath = "/archive/bayki/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

index = page $ PageSpec 
  { pTitle = "Новости Клуба ВИИЯ"
  , pListing = "x:/~listings/index"
  , pPath = "x:/index.htm"
  , pArchPath = "/archive/index/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

orlov = page $ PageSpec 
  { pTitle = "Дружеские шаржи Орлова Владимира Анатольевича В-72"
  , pListing = "x:/~listings/orlov"
  , pPath = "x:/orlov.htm"
  , pArchPath = "/archive/orlov/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

newVIIA = page $ PageSpec 
  { pTitle = "ВИИЯ нового образа"
  , pListing = "x:/~listings/newVIIA"
  , pPath = "x:/newVIIA.htm"
  , pArchPath = "/archive/newVIIA/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

health = page $ PageSpec 
  { pTitle = "ВИИЯ-Здоровье"
  , pListing = "x:/~listings/health"
  , pPath = "x:/health.htm"
  , pArchPath = "/archive/health/"
  , pMpLeft = commonLeft
  , pMpRight = mainRight }

data PageSpec = PageSpec
  { pTitle :: String
  , pListing :: String
  , pPath :: String
  , pArchPath :: String
  , pMpLeft :: [Block]
  , pMpRight :: [Block] }



page PageSpec 
  { pTitle=title
  , pListing=listing
  , pPath=path
  , pArchPath=archPath
  , pMpLeft=mpLeft
  , pMpRight=mpRight 
  } = do

  l <- readListing listing
  let idxl = take 50 l
      arch = groupByMonths l
      archList = Common "Другие страницы архива" 
        (printf "<ul style='list-style-type:none;'>%s</ul>" $ arch >>= f)
        where f (date, list) = wrapLi $ mkArchiveLink archPath date $ showDate date
  createDirectoryIfMissing True $ "x:/" ++ archPath

  runMP path idxl $ do
    mpC <- liftIO $ mapM readHead idxl
    let mpTitle = title
        mpContent = mpC 
          ++ [wrapP $ mkArchiveLink archPath (fst $ head arch) "Архив" | not $ null arch]
    return MainPage {..}

  forM_ arch $ \ (date, list) -> runMP (fileNameFromDate ("x:" ++ archPath) date) list $ do
    let mpTitle = printf "%s. Архив за %s" title $ showDate date
        mpRight = [archList]
    mpContent <- liftIO $ mapM readHead list
    return MainPage {..}


wrapP :: String -> String
wrapP = printf "<p>%s</p>"

wrapLi :: String -> String
wrapLi = printf "<li>%s</li>" 

mkArchiveLink :: FilePath -> Date -> String -> String
mkArchiveLink prefix date text = printf "<a href='%s'>%s</a>" (fileNameFromDate prefix date) text

registerPerson = \ personID -> mapM_ recordDepend $ personFiles personID

personPath :: String -> String
personPath = printf "/people/%s.htm" 

personLink :: String -> String 
personLink fn = printf "<a href='%s'>%s</a>" (personPath fn) fn

people = do
  ppl <- listPeople
  let output = "people.htm"
      fp = ("x:/" ++ output)
  x <- isNeedRebuild fp []
  case x of 
    False -> printf "skipping %s\n" fp
    True -> do                                
      printf "processing %s ... " fp
      hFlush stdout
      r <- runM $ do
        ppl' <- forM ppl $ \ pID -> do
          registerPerson pID
          fn <- fullName pID
          return $ (fn, wrapP $ personLink fn)
        let mpTitle = "Люди ВИИЯ КА"
            mpLeft = commonLeft
            mpRight = mainRight
            mpContent = map snd $ sortBy (compare `on` fst) ppl'
        a <- mkPage $ MainPage{..}
        deps <- A.get depends
        liftIO $ do
          writeFileE utf8 fp a
          updateRebuildInfo fp [] $ S.toList deps
      error . show <++> const (putStrLn "done") $ r

  forM_ ppl $ \ personID -> do
    Person{..} <- readPerson personID
    name <- fullName personID
    page $ PageSpec 
      { pTitle = name
      , pListing = personID ++ "/articles"
      , pPath = "x:" ++ personPath name
      , pArchPath = "/archive" ++ personPath name
      , pMpLeft = commonLeft
      , pMpRight = mainRight }