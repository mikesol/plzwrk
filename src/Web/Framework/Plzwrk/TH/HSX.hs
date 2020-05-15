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

import Control.Monad.Identity (Identity)
import           Control.Applicative            ( (<*)
                                                , (*>)
                                                , (<$>)
                                                , (<$)
                                                )
import Control.Monad.Trans (lift)
import           Control.Monad
import           Control.Monad.Logger
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

hsx :: MonadLoggerIO m => HSXParser m HSX
hsx = tag

tag :: MonadLoggerIO m => HSXParser m HSX
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

elementHSXBody :: MonadLoggerIO m => HSXParser m HSX
elementHSXBody =
  ws
    *> (   try tag
       <|> try haskellCodeNode
       <|> try haskellCodeNodes
       <|> try haskellTxtNode
       <|> text
       <?> "A tag, a piece of code or some text"
       )

endTag :: MonadLoggerIO m => String -> HSXParser m String
endTag str = string "</" *> string str <* char '>'

text :: MonadLoggerIO m => HSXParser m HSX
text = HSXBody <$> many1 (noneOf "><")

stringAttribute :: MonadLoggerIO m => HSXParser m HSXAttribute
stringAttribute = do
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  return $ HSXStringAttribute value

makeBracketed :: MonadLoggerIO m => String -> Bool -> HSXParser m String
makeBracketed cmd contain = do
  let start = "#" <> cmd <> "{"
  let end   = "}#"
  string start
  value <- manyTill anyChar (string end)
  ws
  return $ if contain then start <> value <> end else value

haskellCodeAttr :: MonadLoggerIO m => HSXParser m HSXAttribute
haskellCodeAttr = do
  value <- makeBracketed "c" False
  return $ HSXHaskellCodeAttribute value

haskellCodeNode :: MonadLoggerIO m => HSXParser m HSX
haskellCodeNode = do
  value <- makeBracketed "e" False
  return $ HSXHaskellCode value

haskellCodeNodes :: MonadLoggerIO m => HSXParser m HSX
haskellCodeNodes = do
  value <- makeBracketed "el" False
  return $ HSXHaskellCodeList value

haskellTxtNode :: MonadLoggerIO m => HSXParser m HSX
haskellTxtNode = do
  value <- makeBracketed "t" False
  return $ HSXHaskellText value

haskellTxtAttr :: MonadLoggerIO m => HSXParser m HSXAttribute
haskellTxtAttr = do
  value <- makeBracketed "t" False
  return $ HSXHaskellTxtAttribute value

attribute :: MonadLoggerIO m => HSXParser m (String, HSXAttribute)
attribute = do
  name <- many (noneOf "= />")
  ws
  char '='
  ws
  value <- stringAttribute <|> try haskellCodeAttr <|> haskellTxtAttr
  ws
  return (name, value)

ws :: MonadLoggerIO m => HSXParser m ()
ws = void $ many $ oneOf " \t\r\n"

p :: MonadLoggerIO m => String -> Int -> Int -> HSXParser (NoLoggingT m) HSX
p file line col = do
  updatePosition file line col
  ws
  e <- hsx
  ws
  eof
  return e


parseHSX :: (MF.MonadFail m, MonadLoggerIO m) => (String, Int, Int) -> String -> m HSX
parseHSX (file, line, col) s = do
  res <- runNoLoggingT (runParserT (p file line col) () "" s)
  case res of
    Left err -> MF.fail $ show err
    Right e  -> return e

updatePosition file line col = do
  pos <- getPosition
  setPosition
    $ (flip setSourceName) file
    $ (flip setSourceLine) line
    $ (flip setSourceColumn) col
    $ pos
