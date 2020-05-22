{-# LANGUAGE OverloadedStrings #-}

module Web.Framework.Plzwrk.TH.PWX
  ( PWXAttribute(..)
  , PWX(..)
  , parsePWX
  , parsePWX_
  ------------ for debugging
  , endTag
  , elementPWXBody
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

type PWXParser = ParsecT String ()

data PWXAttribute = PWXStringAttribute String
                 | PWXHaskellCodeAttribute String
                 | PWXHaskellTxtAttribute String deriving (Show, Eq)

data PWX = PWXElement
            { _pwxElement_tag :: String
            , _pwxElement_attributes :: [(String, PWXAttribute)]
            , _pwxElement_children :: [PWX]
            }
            | PWXSelfClosingTag
            { _pwxSelfClosingTag_tag :: String
            , _pwxSelfClosingTag_attributes :: [(String, PWXAttribute)]
            }
            | PWXHaskellCode { _pwxHaskellCode_code :: String }
            | PWXHaskellCodeList { _pwxHaskellCodeList_codeList :: String }
            | PWXHaskellText { _pwxHaskellText_text :: String }
            | PWXBody { _pwxBody_body :: String }
          deriving (Show, Eq)

pwx :: (Monad m) => PWXParser m PWX
pwx = tag

tag :: (Monad m) => PWXParser m PWX
tag = do
  char '<'
  ws
  name <- many (letter <|> digit)
  ws
  attr <- many attribute
  ws
  close <- try (string "/>" <|> string ">")
  if length close == 2
    then return (PWXSelfClosingTag name attr)
    else do
      elementBody <- many elementPWXBody
      endTag name
      ws
      return (PWXElement name attr elementBody)

elementPWXBody :: (Monad m) => PWXParser m PWX
elementPWXBody =
  ws
    *> (   try tag
       <|> try haskellCodeNode
       <|> try haskellCodeNodes
       <|> try haskellTxtNode
       <|> text
       <?> "A tag, a piece of code or some text"
       )

endTag :: (Monad m) => String -> PWXParser m String
endTag str = string "</" *> string str <* char '>'

text :: (Monad m) => PWXParser m PWX
text = PWXBody <$> many1 (noneOf "><")

stringAttribute :: (Monad m) => PWXParser m PWXAttribute
stringAttribute = do
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  return $ PWXStringAttribute value

makeBracketed :: (Monad m) => String -> Bool -> PWXParser m String
makeBracketed cmd contain = do
  let start = "#" <> cmd <> "{"
  let end   = "}#"
  string start
  value <- manyTill anyChar (try (string end))
  ws
  return $ if contain then start <> value <> end else value

haskellCodeAttr :: (Monad m) => PWXParser m PWXAttribute
haskellCodeAttr = do
  value <- makeBracketed "c" False
  return $ PWXHaskellCodeAttribute value

haskellCodeNode :: (Monad m) => PWXParser m PWX
haskellCodeNode = do
  value <- makeBracketed "e" False
  return $ PWXHaskellCode value

haskellCodeNodes :: (Monad m) => PWXParser m PWX
haskellCodeNodes = do
  value <- makeBracketed "el" False
  return $ PWXHaskellCodeList value

haskellTxtNode :: (Monad m) => PWXParser m PWX
haskellTxtNode = do
  value <- makeBracketed "t" False
  return $ PWXHaskellText value

haskellTxtAttr :: (Monad m) => PWXParser m PWXAttribute
haskellTxtAttr = do
  value <- makeBracketed "t" False
  return $ PWXHaskellTxtAttribute value

attribute :: (Monad m) => PWXParser m (String, PWXAttribute)
attribute = do
  name <- many (noneOf "= />")
  ws
  char '='
  ws
  value <- stringAttribute <|> try haskellCodeAttr <|> haskellTxtAttr
  ws
  return (name, value)

ws :: (Monad m) => PWXParser m ()
ws = void $ many $ oneOf " \t\r\n"

parsePWX_ :: (Monad m) => String -> m PWX
parsePWX_ s = do
  res <- runParserT pwx () "" s
  case res of
    Left  err -> error $ show err
    Right e   -> return e

parsePWX :: (Monad m) => (String, Int, Int) -> String -> m PWX
parsePWX (file, line, col) s = do
  res <- runParserT p () "" s
  case res of
    Left  err -> error $ show err
    Right e   -> return e
 where
  p = do
    updatePosition file line col
    ws
    e <- pwx
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
