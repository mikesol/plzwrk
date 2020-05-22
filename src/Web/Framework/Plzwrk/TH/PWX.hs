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

import           Control.Monad.Identity         ( Identity )
import           Control.Applicative            ( (<*)
                                                , (*>)
                                                , (<$>)
                                                , (<$)
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad
import           Control.Monad.Logger
import           Data.Char
import           Data.List                      ( foldl' )
import qualified Data.Text                     as T
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

pwx :: MonadLoggerIO m => PWXParser m PWX
pwx = tag

tag :: MonadLoggerIO m => PWXParser m PWX
tag = do
  lift $ logDebugN "starting tag"
  char '<'
  lift $ logDebugN "consumed <"
  ws
  name <- many (letter <|> digit)
  lift $ logDebugN (T.unwords $ fmap T.pack ["consumed name:", name])
  ws
  attr <- many attribute
  lift $ logDebugN "consumed attributes"
  ws
  close <- try (string "/>" <|> string ">")
  lift $ logDebugN (T.unwords $ fmap T.pack ["consumed close", close])
  if length close == 2
    then return (PWXSelfClosingTag name attr)
    else do
      elementBody <- many elementPWXBody
      endTag name
      ws
      return (PWXElement name attr elementBody)

elementPWXBody :: MonadLoggerIO m => PWXParser m PWX
elementPWXBody =
  ws
    *> (   try tag
       <|> try haskellCodeNode
       <|> try haskellCodeNodes
       <|> try haskellTxtNode
       <|> text
       <?> "A tag, a piece of code or some text"
       )

endTag :: MonadLoggerIO m => String -> PWXParser m String
endTag str = string "</" *> string str <* char '>'

text :: MonadLoggerIO m => PWXParser m PWX
text = PWXBody <$> many1 (noneOf "><")

stringAttribute :: MonadLoggerIO m => PWXParser m PWXAttribute
stringAttribute = do
  lift $ logDebugN "  continuing string attribute"
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  lift $ logDebugN (T.unwords $ fmap T.pack ["finishing attribute", value])
  return $ PWXStringAttribute value

makeBracketed :: MonadLoggerIO m => String -> Bool -> PWXParser m String
makeBracketed cmd contain = do
  lift $ logDebugN $ T.pack ("starting bracketed command " <> cmd)
  let start = "#" <> cmd <> "{"
  let end   = "}#"
  string start
  value <- manyTill anyChar (try $ string end)
  ws
  lift $ logDebugN (T.unwords $ fmap T.pack ["found bracketed command", cmd])
  return $ if contain then start <> value <> end else value

haskellCodeAttr :: MonadLoggerIO m => PWXParser m PWXAttribute
haskellCodeAttr = do
  value <- makeBracketed "c" False
  return $ PWXHaskellCodeAttribute value

haskellCodeNode :: MonadLoggerIO m => PWXParser m PWX
haskellCodeNode = do
  value <- makeBracketed "e" False
  return $ PWXHaskellCode value

haskellCodeNodes :: MonadLoggerIO m => PWXParser m PWX
haskellCodeNodes = do
  value <- makeBracketed "el" False
  return $ PWXHaskellCodeList value

haskellTxtNode :: MonadLoggerIO m => PWXParser m PWX
haskellTxtNode = do
  value <- makeBracketed "t" False
  return $ PWXHaskellText value

haskellTxtAttr :: MonadLoggerIO m => PWXParser m PWXAttribute
haskellTxtAttr = do
  value <- makeBracketed "t" False
  return $ PWXHaskellTxtAttribute value

attribute :: MonadLoggerIO m => PWXParser m (String, PWXAttribute)
attribute = do
  lift $ logDebugN "starting attribute"
  name <- many (noneOf "= />")
  lift $ logDebugN (T.unwords $ fmap T.pack [" attribute name", name])
  ws
  char '='
  ws
  value <- stringAttribute <|> try haskellCodeAttr <|> haskellTxtAttr
  ws
  lift $ logDebugN (T.unwords $ fmap T.pack ["finishing attribute", name])
  return (name, value)

ws :: MonadLoggerIO m => PWXParser m ()
ws = void $ many $ oneOf " \t\r\n"

parsePWX_ :: (MonadLoggerIO m) => String -> m PWX
parsePWX_ s = do
  res <- runParserT pwx () "" s
  case res of
    Left  err -> error $ show err
    Right e   -> return e

parsePWX :: (MonadLoggerIO m) => (String, Int, Int) -> String -> m PWX
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
