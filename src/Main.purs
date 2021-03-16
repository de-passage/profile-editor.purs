module Main where

import Prelude
import Data.Argonaut as A
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Foreign.Object (Object, empty, keys, lookup)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Halogen.VDom.Driver (runUI)
import Marked as Marked

newtype LocStr
  = LocStr { en :: Text En, ja :: Maybe (Text Jp), fr :: Maybe (Text Fr) }

type Dictionary
  = Object LocStr

foreign import kind Language

foreign import data En :: Language

foreign import data Jp :: Language

foreign import data Fr :: Language

type Exists f
  = forall r. (forall a. f a -> r) -> r

newtype Text (a :: Language)
  = Text String

data LProxy (a :: Language)
  = LProxy

en = LProxy :: LProxy En

fr = LProxy :: LProxy Fr

ja = LProxy :: LProxy Jp

fromText :: forall a. Text a -> String
fromText (Text s) = s

instance semigroupText :: Semigroup (Text a) where
  append (Text a) (Text b) = Text (a <> b)

instance monoidText :: Monoid (Text a) where
  mempty = Text mempty

type State
  = { dictionary :: Dictionary, currentKey :: Maybe String, currentEn :: Text En, currentJp :: Maybe (Text Jp), currentFr :: Maybe (Text Fr) }

type ChildSlots
  = ( marked :: Marked.Slot Int )

_marked = SProxy :: SProxy "marked"

data Action
  = TextChanged (State -> State)
  | KeyChanged String

class UpdateState a b where
  updateState :: LProxy a -> b -> State -> State

instance updateStateFr :: UpdateState Fr String where
  updateState _ str = _ { currentFr = Just $ Text str }

instance updateStateEn :: UpdateState En String where
  updateState _ str = _ { currentEn = Text str }

instance updateStateJp :: UpdateState Jp String where
  updateState _ str = _ { currentJp = Just $ Text str }

instance updateStateTextFr :: UpdateState Fr (Maybe (Text Fr)) where
  updateState _ str = _ { currentFr = str }

instance updateStateTextEn :: UpdateState En (Text En) where
  updateState _ str = _ { currentEn = str }

instance updateStateTextJp :: UpdateState Jp (Maybe (Text Jp)) where
  updateState _ str = _ { currentJp = str }

class FromState a b where
  fromState :: LProxy a -> State -> b

instance fromStateFr :: FromState Fr String where
  fromState _ = _.currentFr >>> maybe "" fromText

instance fromStateJa :: FromState Jp String where
  fromState _ = _.currentJp >>> maybe "" fromText

instance fromStateEn :: FromState En String where
  fromState _ = _.currentEn >>> fromText

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
    let
      ks = keys state.dictionary

      ksOptions = map (\k -> HH.option [ HP.selected (maybe false (k == _) state.currentKey) ] [ HH.text k ]) ks

      lngInput :: forall a. UpdateState a String => FromState a String => String -> String -> LProxy a -> Int -> H.ComponentHTML Action ChildSlots m
      lngInput id title lang n =
        HH.div [ HP.class_ BS.formGroup ]
          [ HH.label [ HP.for id ] [ HH.text title ]
          , HH.input [ HP.type_ HP.InputText, HP.id_ id, HP.class_ BS.formControl, HE.onValueChange (\str -> Just (TextChanged $ updateState lang str)) ]
          , HH.slot _marked n Marked.component { text: fromState lang state, id: id } absurd
          ]

      newKey =
        HH.div [ HP.classes [ BS.formGroup ] ]
          [ HH.label [ HP.for "newkey" ] [ HH.text "New Key" ]
          , HH.input [ HP.type_ HP.InputText, HP.id_ "newkey", HE.onValueChange $ Just <<< KeyChanged ]
          ]

      inputs = case state.currentKey of
        Nothing -> [ newKey ]
        Just currKey ->
          [ newKey
          , lngInput "en-input" "English" en 0
          , lngInput "fr-input" "French" fr 1
          , lngInput "ja-input" "Japanese" ja 2
          ]
    in
      HH.div [ HP.class_ BS.container ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.class_ BS.col12 ]
                [ HH.form_
                    [ HH.div [ HP.classes [ BS.formGroup, BS.row ] ]
                        ( [ HH.label [ HP.for "keyselection" ] [ HH.text "Keys" ]
                          , HH.select [ HP.id_ "keyselection", HP.class_ BS.formControl ] ksOptions
                          ]
                            <> inputs
                        )
                    ]
                ]
            ]
        ]

  handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output m Unit
  handleAction action = case action of
    TextChanged change -> do
      H.liftEffect $ log "Text changed"
      H.modify_ change
    KeyChanged key -> do
      dic <- H.gets _.dictionary
      H.modify_ _ { currentKey = Just key }
      case lookup key dic of
        Just (LocStr loc) -> H.modify_ $ updateState en loc.en >>> updateState fr loc.fr >>> updateState ja loc.ja
        Nothing -> pure unit
