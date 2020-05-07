module Web.Framework.Plzwrk.TH.QuoteHSX
  ( hsx
  , hsx'
  , hmFromList
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

hmFromList :: (Eq k, H.Hashable k) => [(k, v)] -> HM.HashMap k v
hmFromList = HM.fromList

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

hsxToExpQ :: Bool -> HSX -> TH.Q TH.Exp
hsxToExpQ lam (HSXHaskellCode y         ) = haskize y
hsxToExpQ lam (HSXHaskellText y         ) = wrapInLambda True $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (haskize y)
hsxToExpQ lam (HSXElement tag attrs elts) = wrapInLambda lam $ foldl
  TH.appE
  (TH.conE (TH.mkName "PwElement"))
  [ TH.litE (TH.StringL tag)
  , TH.listE (fmap hsxAttributeToExpQ attrs)
  , TH.listE (fmap (\x -> hsxToExpQ True x) elts)
  ]
hsxToExpQ lam (HSXSelfClosingTag tag attrs) = wrapInLambda lam $ foldl
  TH.appE
  (TH.conE (TH.mkName "PwElement"))
  [ (TH.litE (TH.StringL tag))
  , TH.listE (fmap hsxAttributeToExpQ attrs)
  , (TH.conE (TH.mkName "[]"))
  ]
hsxToExpQ lam (HSXBody b) = wrapInLambda lam
  $ TH.appE (TH.conE (TH.mkName "PwTextNode")) (TH.litE (TH.StringL b))

quoteExprExp b s = do
  pos    <- getPosition
  result <- parseHSX pos s
  hsxToExpQ b result

getPosition = fmap transPos TH.location where
  transPos loc =
    (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
