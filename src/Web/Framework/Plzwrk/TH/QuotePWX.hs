module Web.Framework.Plzwrk.TH.QuotePWX
  ( pwx
  , pwx'
  , plusplus
  )
where

import           Data.List.Split
import qualified Data.Hashable                 as H
import qualified Language.Haskell.TH           as TH
import           Language.Haskell.Meta.Parse    ( parseExp )
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Web.Framework.Plzwrk.TH.PWX
import           Web.Framework.Plzwrk.Base
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S


pwx :: QuasiQuoter
pwx = QuasiQuoter { quoteExp  = quoteExprExp True
                  , quotePat  = undefined
                  , quoteDec  = undefined
                  , quoteType = undefined
                  }

pwx' :: QuasiQuoter
pwx' = QuasiQuoter { quoteExp  = quoteExprExp False
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

pwxAttributeToExpQ :: (String, PWXAttribute) -> TH.Q TH.Exp
pwxAttributeToExpQ (k, PWXStringAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE
    [TH.varP (TH.mkName "_")]
    (TH.appE (TH.conE (TH.mkName "PwTextAttribute")) (TH.litE (TH.StringL v)))
  ]
pwxAttributeToExpQ (k, PWXHaskellCodeAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE [TH.varP (TH.mkName "_")]
            (TH.appE (TH.conE (TH.mkName "PwFunctionAttribute")) (haskize v))
  ]
pwxAttributeToExpQ (k, PWXHaskellTxtAttribute v) = TH.tupE
  [ TH.litE (TH.StringL k)
  , TH.lamE [TH.varP (TH.mkName "_")]
            (TH.appE (TH.conE (TH.mkName "PwTextAttribute")) (haskize v))
  ]


wrapInLambda :: Bool -> TH.Q TH.Exp -> TH.Q TH.Exp
wrapInLambda True  e = TH.lamE [TH.varP (TH.mkName "_")] e
wrapInLambda False e = e

asList :: Bool -> TH.Q TH.Exp -> TH.Q TH.Exp
asList b e = if b then TH.listE [e] else e

pwxToExpQ :: Bool -> Bool -> PWX -> TH.Q TH.Exp
pwxToExpQ lam returnAsList (PWXHaskellCode y) = asList returnAsList (haskize y)
pwxToExpQ lam returnAsList (PWXHaskellCodeList y) = haskize y
pwxToExpQ lam returnAsList (PWXHaskellText y) = asList
  returnAsList
  (wrapInLambda True $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (haskize y))
pwxToExpQ lam returnAsList (PWXElement tag attrs elts) = asList
  returnAsList
  (wrapInLambda lam $ foldl
    TH.appE
    (TH.conE (TH.mkName "PwElement"))
    [ TH.litE (TH.StringL tag)
    , TH.listE (fmap pwxAttributeToExpQ attrs)
    , foldl
      TH.appE
      (TH.varE (TH.mkName "foldr"))
      [ TH.varE (TH.mkName "plusplus")
      , TH.conE (TH.mkName "[]")
      , TH.listE (fmap (pwxToExpQ True True) elts)
      ]
    ]
  )
pwxToExpQ lam returnAsList (PWXSelfClosingTag tag attrs) = asList
  returnAsList
  (wrapInLambda lam $ foldl
    TH.appE
    (TH.conE (TH.mkName "PwElement"))
    [ TH.litE (TH.StringL tag)
    , TH.listE (fmap pwxAttributeToExpQ attrs)
    , TH.conE (TH.mkName "[]")
    ]
  )
pwxToExpQ lam returnAsList (PWXBody b) = asList
  returnAsList
  ( wrapInLambda lam
  $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (TH.litE (TH.StringL b))
  )

quoteExprExp b s = do
  pos    <- getPosition
  result <- parsePWX pos s
  pwxToExpQ b False result

getPosition = fmap transPos TH.location where
  transPos loc =
    (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
