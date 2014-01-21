module PlainTemplate.Parser 
    ( Tag(..)
    , body
    , runP
    , runPT
    , Body
    , Pos
    )
where
import Text.Parsec.Pos
import Text.Parsec hiding ((<|>))
import Library hiding(many)

data Tag 
  = Tag Pos String [String] [Either String Tag]
    deriving Show

type Body = [Either String Tag]
type Pos = (SourcePos,SourcePos)
type Parser = Parsec String ()

escapedChars str = msum
  [ noneOf str
  , char (head str) >> (oneOf str <|> return '\\') ]

tagHead :: Parser (String, [String])
tagHead = do
  name <- many1 (letter <|> digit)
  parts <- many $ do
    char ':'
    many $ escapedChars "\\|:]"
  return (name,parts)

tag :: Parser Tag
tag = do
  p <- getPosition
  char '['
  (n,ps) <- tagHead
  b <- (char '|' >> body) <|> return []
  char ']'
  p2 <- getPosition
  return $ Tag (p,p2) n ps b

body :: Parser Body
body = do
  many $ msum 
    [ Right `liftM` tag
    , Left `liftM` bodyString ]
  
bodyString :: Parser String
bodyString = do
  many1 $ escapedChars "\\[]"
