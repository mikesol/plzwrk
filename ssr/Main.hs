{-# LANGUAGE CPP               #-}

import           Web.Framework.Plzwrk
#if defined(PLZWRK_ENABLE_ASTERIUS)
import           Web.Framework.Plzwrk.Asterius
import           Web.Framework.Plzwrk.Tag (p__)
# else
import           Web.Scotty
# endif
{-# LANGUAGE QuasiQuotes #-}

import           NeatInterpolation

main :: IO ()
main = do
#if defined(PLZWRK_ENABLE_ASTERIUS)
  -- build the DOM
  browser <- asteriusBrowser
  plzwrk'_ topL browser
# else
  -- build the server
  scotty 3000 $
    get "/" $ do
      html $ domf (toHTML topL 0)
# endif

footerF = footer
  (   wId "footer-2"
  <.> wClasses ["section", "text-center", "black-flat"]
  <.> wAttr "activepage" "Landing"
  )
  [ div
      (wClasses ["container col-12"])
      [ div_
          [ fmap
              (\(x, y) -> a
                (   wClasses ["socialicons", "accent-bg", x]
                <.> wAttr "href"   y
                <.> wAttr "target" "_blank"
                )
                []
              )
              [ ("twitter" , "https://twitter.com/MeeshkanML")
              , ("github"  , "https://github.com/meeshkan/plzwrk")
              , ("linkedin", "https://linkedin.com/company/meeshkan/")
              ]
          ]
          div
          (wClass "mt50")
          [span (wClasses ["span", "secondary-color"]) [txt "© 2020 Meeshkan"]]
      ]
  ]

rocketPath = "M20 22.494v1a.5.5 0 0 1-1 0v-1a.5.5 0 1 1 1 0zm-14.5-.5a.5.5 0 0 0-.5.5v1a.5.5 0 0 0 1 0v-1a.5.5 0 0 0-.5-.5zm7 1a10.924 10.924 0 0 1-2.494-.391l-.27 1.76a.528.528 0 0 0 .097.454.498.498 0 0 0 .387.183h4.56a.5.5 0 0 0 .49-.598l-.275-1.8a10.928 10.928 0 0 1-2.495.392zm9.06-7.44L18 11.995v9h2.5a1.5 1.5 0 0 0 1.5-1.5v-2.879a1.5 1.5 0 0 0-.44-1.06zM17 5.995v15a18.472 18.472 0 0 1-4 .974v-7.474a.5.5 0 0 0-1 0v7.474a18.47 18.47 0 0 1-4-.974v-15C8 3.67 11.553.244 12.5 0c.947.244 4.5 3.67 4.5 5.994zm-2.168 1.127A4.374 4.374 0 0 0 12.5 5.994a4.113 4.113 0 0 0-2.343 1.136.5.5 0 0 0 .686.729 3.213 3.213 0 0 1 1.657-.865 3.417 3.417 0 0 1 1.668.874.5.5 0 0 0 .664-.748zM3 16.614v2.88a1.5 1.5 0 0 0 1.5 1.5H7v-9l-3.56 3.56a1.5 1.5 0 0 0-.44 1.06z"

reasonsF = section
  (wId "feature-1" <.> wClasses ["section", "black-white"] <.> wAttr
    "activepage"
    "Landing"
  )
  [ div
      (wClasses ["container-lrg", "flex"])
      fmap
      (\(title, descr, myPath) -> div
        (wClasses $ words "col-4 text-center flex flex-column center-horizontal"
        )
        [ i
          (wClasses $ words
            "icon mobile-center-icon secondary-bg launchaco-builder-hoverable"
          )
          [ svg
              (   wClass "svg-fill"
              <.> wAttrs
                    [ ("xmlns"  , "http://www.w3.org/2000/svg")
                    , ("viewBox", "0 0 25 25")
                    ]
              )
              [path (wAttr "d" myPath) []]
          ]
        , h3 () [txt title]
        , p () [txt descr]
        ]
      )
      [ ( "Server-side rendering"
        , div_
          [ span__ "Like this page, for example - check out the "
          , a (wAttr "href" "https://github.com/meeshkan/plzwrk")
              [txt "source!"]
          ]
        , rocketPath
        )
      , ( "Written in Haskell"
        , txt "Typesafe code that compiles directly from Haskell using ahc."
        , rocketPath
        )
      , ( "Automatic reloading"
        , txt "Use nodemon to update your app as you're coding."
        , rocketPath
        )
      ]
  ]

bannerF = section
  (wClass "secion")
  [ div
      (wClasses $ words "container mb40")
      [ div
          (wClasses $ words "col-12 text-center")
          [ h1
            ( wClasses
            $ words "heading-lrg primary-color launchaco-builder-hoverable"
            )
            [txt "A front-end framework"]
          , h2
            (wClasses $ words
              "subheading secondary-color mt20 launchaco-builder-hoverable"
            )
            [txt "Build web apps using Haskell"]
          , div
            (wClass "mt40")
            [ button
                ( wClasses
                $ words
                    "button mobile-text-center mt10 launchaco-builder-hoverable mobile-text-center accent-bg primary-color"
                )
                [span__ "Click here to change stuff"]
            ]
          ]
      ]
  ]

headerF = header
  (wClass "header")
  [ div
      (wClass "container-lrg")
      [ div
          (wClasses $ words "flex col-12 spread")
          [ a
              (   wClasses
              $   words "logo primary-color launchaco-builder-hoverable logo"
              <.> wAttr "href" "https://github.com/meeshkan/plzwrk"
              )
              [txt "plzwrk"]
          ]
      ]
  ]

topL = div_ [div (wClass "black-flat") [headerF, bannerF], reasonsF, footerF]

domf a = [text|
  <!DOCTYPE html>
  <html>
    <head>
      <title>plzwrk</title>
      <meta charSet="utf-8">
      <meta name="viewport" content="width=device-width,initial-scale=1">
      <meta name="description" content="A front-end framework">
      <meta property="og:image" content="">
      <link rel="shortcut icon" href="https://www.launchaco.com/static/favicon.ico" />
      <link href="https://fonts.googleapis.com/css?family=Montserrat:400,700" rel="stylesheet">
      <link rel="stylesheet" href="https://www.launchaco.com/static/AllTemplates.min.css">
    </head>
    <body class="font-friendly">
      $a
    </body>
  </html>
|]