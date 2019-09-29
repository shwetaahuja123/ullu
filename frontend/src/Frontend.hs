{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import qualified Data.Map     as M
import qualified Data.Bool    as B

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "app"
      styleSheet $ static @"style.css"
      styleSheet "//fonts.googleapis.com/css?family=Nothing+You+Could+Do"
  , _frontend_body = elAttr "div" ("id" =: "todo-background") $ do
                       rec
                         let enter         = keypress Enter input
                             ev1           = leftmost [ True <$ enter
                                              , False <$ clickEv
                                              ]
                             modAttrsEv    = getModAttrs <$> ev1
                             getModAttrs :: B.Bool -> M.Map AttributeName (Maybe T.Text)
                             getModAttrs b = "style" =: Just ("display: " <> B.bool "inline" "none" b)
                         input <- inputElement $
                           def
                             & inputElementConfig_setValue .~ fmap (const "") enter
                             & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
                                 ("id" =: "todo-input"
                                 <> "placeholder" =: "enter hunter task")
                             & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrsEv


                         let taskEv  = tagPromptlyDyn (_inputElement_value input) enter
                             clickEv = domEvent Click el
                             ev      = leftmost [ taskEv
                                                , "" <$ clickEv
                                                ]
                         (el, _) <- elAttr' "div" ("class" =: "task") $ dynText =<< holdDyn "" ev

                       pure()
  }

--  styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => T.Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink

