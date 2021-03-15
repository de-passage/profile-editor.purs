module Main where

import Halogen.Aff
import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Foreign.Object (Object, empty)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy)

newtype LocStr
  = LocStr { en :: String, ja :: Maybe String, fr :: Maybe String }

type Dictionary
  = Object LocStr

foreign import kind Language

foreign import data En :: Language

foreign import data Jp :: Language

foreign import data Fr :: Language

newtype Text (a :: Language)
  = Text String

instance semigroupText :: Semigroup (Text a) where
  append (Text a) (Text b) = Text (a <> b)

instance monoidText :: Monoid (Text a) where
  mempty = Text mempty

type State
  = { dictionary :: Dictionary, currentKey :: String, currentEn :: Text En, currentJp :: Maybe (Text Jp), currentFr :: Maybe (Text Fr) }

type ChildSlots
  = ()

type Action
  = Unit

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    H.liftEffect $ log "done"
    runUI editor unit body

editor :: forall q o m i. MonadEffect m => H.Component HH.HTML q i o m
editor =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ =
    { dictionary: empty
    , currentKey: mempty
    , currentEn: mempty
    , currentJp: mempty
    , currentFr: mempty
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div [ HP.class_ BS.container ]
      [ HH.div [ HP.class_ BS.row ]
          [ HH.form_
              [ HH.div [ HP.class_ BS.dropdown ]
                  [ HH.button [ HP.classes [ BS.btn, BS.btnSecondary ] ] [ HH.text state.currentKey ]
                  ]
              ]
          ]
      ]

  handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output m Unit
  handleAction _ = pure unit
