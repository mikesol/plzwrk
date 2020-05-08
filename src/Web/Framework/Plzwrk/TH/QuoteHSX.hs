module Web.Framework.Plzwrk.TH.QuoteHSX
  ( hsx
  , hsx'
  , plusplus
  )
where

import           Data.List.Split
import qualified Data.Hashable                 as H
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.Meta.Parse    ( parseExp )
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Web.Framework.Plzwrk.TH.HSX
import           Web.Framework.Plzwrk.Base
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S


hsx :: QuasiQuoter
hsx = QuasiQuoter { quoteExp  = quoteExprExp True
                  , quotePat  = undefined
                  , quoteDec  = undefined
                  , quoteType = undefined
                  }

hsx' :: QuasiQuoter
hsx' = QuasiQuoter { quoteExp  = quoteExprExp False
                   , quotePat  = undefined
                   , quoteDec  = undefined
                   , quoteType = undefined
                   }

haskize y = either
  (\s -> TH.appE
    (TH.varE (TH.mkName "error"))
    (TH.litE (TH.StringL $ "Could not parse: " <> y <> " due to error " <> s))
  )
  returnQ
  (parseExp y)

plusplus :: [a] -> [a] -> [a]
plusplus = (++)

hsxAttributeToExpQ :: (String, HSXAttribute) -> TH.Q TH.Exp
hsxAttributeToExpQ (k, HSXStringAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE
    [TH.varP (TH.mkName "_")]
    (TH.appE (TH.conE (TH.mkName "PwTextAttribute")) (TH.litE (TH.StringL v)))
  ]
hsxAttributeToExpQ (k, HSXHaskellCodeAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE [TH.varP (TH.mkName "_")]
            (TH.appE (TH.conE (TH.mkName "PwFunctionAttribute")) (haskize v))
  ]
hsxAttributeToExpQ (k, HSXHaskellTxtAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE [TH.varP (TH.mkName "_")]
            (TH.appE (TH.conE (TH.mkName "PwTextAttribute")) (haskize v))
  ]


wrapInLambda :: Bool -> TH.Q TH.Exp -> TH.Q TH.Exp
wrapInLambda True  e = TH.lamE [TH.varP (TH.mkName "_")] e
wrapInLambda False e = e

asList :: Bool -> TH.Q TH.Exp -> TH.Q TH.Exp
asList b e = if b then TH.listE [e] else e

hsxToExpQ :: Bool -> Bool -> HSX -> TH.Q TH.Exp
hsxToExpQ lam returnAsList (HSXHaskellCode y) = asList returnAsList (haskize y)
hsxToExpQ lam returnAsList (HSXHaskellCodeList y) = haskize y
hsxToExpQ lam returnAsList (HSXHaskellText y) = asList
  returnAsList
  (wrapInLambda True $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (haskize y))
hsxToExpQ lam returnAsList (HSXElement tag attrs elts) = asList
  returnAsList
  (wrapInLambda lam $ foldl
    TH.appE
    (TH.conE (TH.mkName "PwElement"))
    [ TH.litE (TH.StringL tag)
    , TH.listE (fmap hsxAttributeToExpQ attrs)
    , foldl TH.appE (TH.varE (TH.mkName "foldr")) [(TH.varE (TH.mkName "plusplus")), (TH.conE (TH.mkName "[]")), TH.listE (fmap (\x -> hsxToExpQ True True x) elts)]
    ]
  )
hsxToExpQ lam returnAsList (HSXSelfClosingTag tag attrs) = asList
  returnAsList
  (wrapInLambda lam $ foldl
    TH.appE
    (TH.conE (TH.mkName "PwElement"))
    [ (TH.litE (TH.StringL tag))
    , TH.listE (fmap hsxAttributeToExpQ attrs)
    , (TH.conE (TH.mkName "[]"))
    ]
  )
hsxToExpQ lam returnAsList (HSXBody b) = asList
  returnAsList
  ( wrapInLambda lam
  $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (TH.litE (TH.StringL b))
  )

quoteExprExp b s = do
  pos    <- getPosition
  result <- parseHSX pos s
  hsxToExpQ b False result

getPosition = fmap transPos TH.location where
  transPos loc =
    (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
