{-# LANGUAGE OverloadedStrings #-}

module Web.Framework.Plzwrk.TH.HSX
  ( HSXAttribute(..)
  , HSX(..)
  , parseHSX
  , parseHSX_
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

type HSXParser = ParsecT String ()

data HSXAttribute = HSXStringAttribute String
                 | HSXHaskellCodeAttribute String
                 | HSXHaskellTxtAttribute String deriving (Show, Eq)

data HSX = HSXElement
            { _hsxElement_tag :: String
            , _hsxElement_attributes :: [(String, HSXAttribute)]
            , _hsxElement_children :: [HSX]
            }
            | HSXSelfClosingTag
            { _hsxSelfClosingTag_tag :: String
            , _hsxSelfClosingTag_attributes :: [(String, HSXAttribute)]
            }
            | HSXHaskellCode { _hsxHaskellCode_code :: String }
            | HSXHaskellCodeList { _hsxHaskellCodeList_codeList :: String }
            | HSXHaskellText { _hsxHaskellText_text :: String }
            | HSXBody { _hsxBody_body :: String }
          deriving (Show, Eq)

hsx :: MonadLoggerIO m => HSXParser m HSX
hsx = tag

tag :: MonadLoggerIO m => HSXParser m HSX
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
  lift $ logDebugN "  continuing string attribute"
  char '"'
  value <- many (noneOf ['"'])
  char '"'
  lift $ logDebugN (T.unwords $ fmap T.pack ["finishing attribute", value])
  return $ HSXStringAttribute value

makeBracketed :: MonadLoggerIO m => String -> Bool -> HSXParser m String
makeBracketed cmd contain = do
  lift $ logDebugN $ T.pack ("starting bracketed command " <> cmd)
  let start = "#" <> cmd <> "{"
  let end   = "}#"
  string start
  value <- manyTill anyChar (try $ string end)
  ws
  lift $ logDebugN (T.unwords $ fmap T.pack ["found bracketed command", cmd])
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

ws :: MonadLoggerIO m => HSXParser m ()
ws = void $ many $ oneOf " \t\r\n"

parseHSX_ :: (MonadLoggerIO m) => String -> m HSX
parseHSX_ s = do
  res <- runParserT hsx () "" s
  case res of
    Left  err -> error $ show err
    Right e   -> return e

parseHSX :: (MonadLoggerIO m) => (String, Int, Int) -> String -> m HSX
parseHSX (file, line, col) s = do
  res <- runParserT p () "" s
  case res of
    Left  err -> error $ show err
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
