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

hsx :: Parser HSX
hsx = tag

tag = do
  char '<'
  ws
  name <- many (letter <|> digit)
  ws
  attr <- many attribute
  ws
  close <- try (string "/>" <|> string ">")
  if (length close) == 2
    then return (HSXSelfClosingTag name attr)
    else do
      elementBody <- many elementHSXBody
      endTag name
      ws
      return (HSXElement name attr elementBody)

elementHSXBody =
  ws
    *> (   try tag
       <|> try haskellCodeNode
       <|> try haskellCodeNodes
       <|> try haskellTxtNode
       <|> text
       <?> "A tag, a piece of code or some text"
       )

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'

text = HSXBody <$> many1 (noneOf "><")

stringAttribute = do
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  return $ HSXStringAttribute value

haskellTxtAttr = do
  string "#t{"
  value <- manyTill anyChar (string "}#")
  ws
  return $ HSXHaskellTxtAttribute value

makeBracketed cmd contain = do
  let start = ("#" <> cmd <> "{")
  let end   = "}#"
  string start
  value <- manyTill anyChar (string end)
  ws
  return $ if (contain) then start <> value <> end else value

haskellCodeAttr = do
  value <- makeBracketed "c" False
  return $ HSXHaskellCodeAttribute value

haskellCodeNode :: Parser HSX
haskellCodeNode = do
  value <- makeBracketed "e" False
  return $ HSXHaskellCode value

haskellCodeNodes :: Parser HSX
haskellCodeNodes = do
  value <- makeBracketed "el" False
  return $ HSXHaskellCodeList value

haskellTxtNode :: Parser HSX
haskellTxtNode = do
  value <- makeBracketed "t" False
  return $ HSXHaskellText value

attribute = do
  name <- many (noneOf "= />")
  ws
  char '='
  ws
  value <- stringAttribute <|> (try haskellCodeAttr) <|> haskellTxtAttr
  ws
  return (name, value)

ws :: Parser ()
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
