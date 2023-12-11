module Main where

import Prelude

import Control.Monad.ST.Global (toEffect)
import Data.Array ((..))
import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, fromMaybe', maybe)
import Data.String (codePointFromChar)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Deku.DOM (text_)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect as DE
import Deku.Hooks (useDyn, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import FRP.Event (bindToEffect) as Event
import FRP.Event.Class (once) as Event
import FRP.Poll (Poll)
import FRP.Poll as Poll

main :: Effect Unit
main = do
  _ /\ push /\ poll <- toEffect $ DE.useHot [ "A", "B" ] -- DE.useState [ "A", "B" ] --  -}  DE.useState' --  Poll.create
  -- launchAff_ do
  --   -- delay (Milliseconds 0.0)
  --   -- liftEffect (push [ "A", "B" ])
  --   -- delay (Milliseconds 1000.0)
  --   liftEffect (push [ "X", "Y" ])
  -- -- delay (Milliseconds 1000.0)
  -- -- liftEffect (push [ "M", "N" ])
  runInBody Deku.do
    pushItem /\ items <- useState'
    D.table_
      [ D.thead_
          [ D.tr_
              [ D.th_
                  [ D.button
                      [ DL.click $ poll <#> \arr _ -> do
                          candidate <- spy "Add:candidate" <<< maybe "!" (String.singleton <<< codePointFromChar) <<< fromCharCode <$> randomInt (toCharCode 'A') (toCharCode 'Z')
                          let idx = spy "Add:fromMaybe" $ fromMaybe' (\_ -> Array.length arr) $ spy "Add:findIndex" $ Array.findIndex (candidate < _) arr
                          maybe (pure unit) push $ spy "Add:new arr" $ Array.insertAt idx candidate arr
                          pushItem (idx /\ candidate)
                      ]
                      [ text_ "Add" ]
                  ]
              ]
          ]
      , D.tbody_
          [ Deku.do
              let
                arrayPoll = Event.once poll # bindPollToEffect \arr -> launchAff_ do
                  -- Console.debug "before delay"
                  delay $ Milliseconds 0.0
                  -- Console.debug "after delay"
                  liftEffect $ Array.zip (0 .. (Array.length arr - 1)) arr `for_` pushItem

              arrayPoll <#~> \_ -> Deku.do
                { value: item, sendTo, remove } <- useDyn items
                D.tr_
                  [ D.td_
                      [ D.button
                          [ DL.click $ poll <#> \arr _ -> do
                              let newArr = spy "Remove:result arr" $ fromMaybe arr $ spy "Remove:deleted arr" $ flip Array.deleteAt arr =<< spy "Remove:findlastIndex" (Array.findLastIndex (item == _) $ spy "Remove:before arr" arr)
                              push newArr
                              remove
                          ]
                          [ text_ "Remove" ]
                      ]
                  , D.td__ item
                  ]
          ]
      ]

bindPollToEffect :: forall a b. (a -> Effect b) -> Poll a -> Poll b
bindPollToEffect eff poll = poll # Poll.dredge (_ `Event.bindToEffect` eff)
