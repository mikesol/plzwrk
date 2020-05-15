module Web.Framework.Plzwrk.TH.HSX
  ( HSXAttribute(..)
  , HSX(..)
  , parseHSX
  ------------ for debugging
  , endTag
  , elementHSXBody
  , attribute
  , tag
  , text
  , haskellCodeNodes
  , haskellTxtAttr
  , haskellTxtNode
  , haskellCodeNode
  )
where

import           Control.Applicative            ( (<*)
                                                , (*>)
                                                , (<$>)
                                                , (<$)
                                                )
import           Control.Monad                  ( void )
import qualified Control.Monad.Fail            as MF
import           Data.Char
import           Data.List                      ( foldl' )
import           Text.Parsec
import           Text.Parsec.String

type HSXParser = ParsecT String ()

data HSXAttribute = HSXStringAttribute String
                 | HSXHaskellCodeAttribute String
                 | HSXHaskellTxtAttribute String deriving (Show, Eq)

data HSX =  HSXElement String [(String, HSXAttribute)] [HSX]
          | HSXSelfClosingTag String [(String, HSXAttribute)]
          | HSXHaskellCode String
          | HSXHaskellCodeList String
          | HSXHaskellText String
          | HSXBody String
        deriving (Show, Eq)

hsx :: Monad m => HSXParser m HSX
hsx = tag

tag :: Monad m => HSXParser m HSX
tag = do
  char '<'
  ws
  name <- many (letter <|> digit)
  ws
  attr <- many attribute
  ws
  close <- try (string "/>" <|> string ">")
  if length close == 2
    then return (HSXSelfClosingTag name attr)
    else do
      elementBody <- many elementHSXBody
      endTag name
      ws
      return (HSXElement name attr elementBody)

elementHSXBody :: Monad m => HSXParser m HSX
elementHSXBody =
  ws
    *> (   try tag
       <|> try haskellCodeNode
       <|> try haskellCodeNodes
       <|> try haskellTxtNode
       <|> text
       <?> "A tag, a piece of code or some text"
       )

endTag :: Monad m => String -> HSXParser m String
endTag str = string "</" *> string str <* char '>'

text :: Monad m => HSXParser m HSX
text = HSXBody <$> many1 (noneOf "><")

stringAttribute :: Monad m => HSXParser m HSXAttribute
stringAttribute = do
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  return $ HSXStringAttribute value

makeBracketed :: Monad m => String -> Bool -> HSXParser m String
makeBracketed cmd contain = do
  let start = "#" <> cmd <> "{"
  let end   = "}#"
  string start
  value <- manyTill anyChar (string end)
  ws
  return $ if contain then start <> value <> end else value

haskellCodeAttr :: Monad m => HSXParser m HSXAttribute
haskellCodeAttr = do
  value <- makeBracketed "c" False
  return $ HSXHaskellCodeAttribute value

haskellCodeNode :: Monad m => HSXParser m HSX
haskellCodeNode = do
  value <- makeBracketed "e" False
  return $ HSXHaskellCode value

haskellCodeNodes :: Monad m => HSXParser m HSX
haskellCodeNodes = do
  value <- makeBracketed "el" False
  return $ HSXHaskellCodeList value

haskellTxtNode :: Monad m => HSXParser m HSX
haskellTxtNode = do
  value <- makeBracketed "t" False
  return $ HSXHaskellText value

haskellTxtAttr :: Monad m => HSXParser m HSXAttribute
haskellTxtAttr = do
  value <- makeBracketed "t" False
  return $ HSXHaskellTxtAttribute value

attribute :: Monad m => HSXParser m (String, HSXAttribute)
attribute = do
  name <- many (noneOf "= />")
  ws
  char '='
  ws
  value <- stringAttribute <|> try haskellCodeAttr <|> haskellTxtAttr
  ws
  return (name, value)

ws :: Monad m => HSXParser m ()
ws = void $ many $ oneOf " \t\r\n"

parseHSX :: MF.MonadFail m => (String, Int, Int) -> String -> m HSX
parseHSX (file, line, col) s = case runParser p () "" s of
  Left  err -> MF.fail $ show err
  Right e   -> return e
 where
  p = do
    updatePosition file line col
    ws
    e <- hsx
    ws
    eof
    return e

updatePosition file line col = do
  pos <- getPosition
  setPosition
    $ (flip setSourceName) file
    $ (flip setSourceLine) line
    $ (flip setSourceColumn) col
    $ pos
