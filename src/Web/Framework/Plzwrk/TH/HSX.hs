module Web.Framework.Plzwrk.TH.HSX
  ( Attribute(..)
  , XML(..)
  , parseXML
  , AttrName
  , AttrVal
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

type AttrName = String
type AttrVal = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show, Eq)

data XML =  Element String [Attribute] [XML]
          | SelfClosingTag String [Attribute]
          | Body String
        deriving (Show, Eq)

parseXML :: String -> Either ParseError XML
parseXML = parse xml "xml"

xml :: Parser XML
xml = ws *> tag

tag = do
  char '<'
  ws
  name <- many (letter <|> digit)
  ws
  attr <- many attribute
  ws
  close <- try (string "/>" <|> string ">")
  if (length close) == 2
  then return (SelfClosingTag name attr)
  else do 
        elementBody <- many elementBody
        endTag name
        ws
        return (Element name attr elementBody)

elementBody = ws *> try tag <|> text

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'

text = Body <$> many1 (noneOf "><")

attribute = do
  name <- many (noneOf "= />")  
  ws
  char '='
  ws
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  ws
  return (Attribute (name,value))

ws :: Parser ()
ws = void $ many $ oneOf " \t\r\n"