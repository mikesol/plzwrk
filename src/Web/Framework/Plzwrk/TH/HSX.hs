module Web.Framework.Plzwrk.TH.HSX
  ( HSXAttribute(..)
  , HSX(..)
  , parseHSX
  )
where

import           Control.Applicative            ( (<*)
                                                , (*>)
                                                , (<$>)
                                                , (<$)
                                                )
import           Control.Monad                  ( void )
import           Data.Char
import           Data.List                      ( foldl' )
import           Text.Parsec
import           Text.Parsec.String

data HSXAttribute = HSXStringAttribute String
                 | HSXHaskellCodeAttribute String deriving (Show, Eq)

data HSX =  HSXElement String [(String, HSXAttribute)] [HSX]
          | HSXSelfClosingTag String [(String, HSXAttribute)]
          | HSXHaskellCode String
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
        elementHSXBody <- manyTill elementHSXBody (endTag name)
        ws
        return (HSXElement name attr elementHSXBody)

elementHSXBody = ws *> (try tag <|> try haskellCodeNode <|> text)

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'

text = HSXBody <$> many1 (noneOf "><")

stringAttribute = do
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  return $ HSXStringAttribute value

haskellCodeAttr = do
  string "#{"
  value <- manyTill anyChar (string "}#")
  ws
  return $ HSXHaskellCodeAttribute value

haskellCodeNode :: Parser HSX
haskellCodeNode = do
  string "#{"
  value <- manyTill anyChar (string "}#")
  ws
  return $ HSXHaskellCode value


attribute = do
  name <- many (noneOf "= />")  
  ws
  char '='
  ws
  value <- stringAttribute <|> haskellCodeAttr
  ws
  return (name, value)

ws :: Parser ()
ws = void $ many $ oneOf " \t\r\n"

parseHSX :: MonadFail m => (String, Int, Int) -> String -> m HSX
parseHSX (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do updatePosition file line col
           ws
           e <- hsx
           ws
           eof
           return e

updatePosition file line col = do
   pos <- getPosition
   setPosition $
     (flip setSourceName) file $
     (flip setSourceLine) line $
     (flip setSourceColumn) col $
     pos
