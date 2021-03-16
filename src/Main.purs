module Main where

import Prelude
import Data.Argonaut ((.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Foreign.Object (Object, delete, empty, insert, keys, lookup, member)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Halogen.VDom.Driver (runUI)
import Marked as Marked
import Web.Event.Event (preventDefault)
import Web.Event.Event as W
import Web.UIEvent.MouseEvent (toEvent)

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
  = { dictionary :: Dictionary
    , currentKey :: Maybe String
    , currentEn :: Text En
    , currentJp :: Maybe (Text Jp)
    , currentFr :: Maybe (Text Fr)
    , sourceFile :: String
    }

type ChildSlots
  = ( marked :: Marked.Slot Int )

_marked = SProxy :: SProxy "marked"

data Action
  = TextChanged (State -> State)
  | KeyChanged String
  | Save String
  | Delete String
  | PreventDefault W.Event (Maybe Action)
  | SourceFile String
  | LoadFile
  | GenerateFile

class UpdateState a b where
  updateState :: LProxy a -> b -> State -> State

instance updateStateFr :: UpdateState Fr String where
  updateState _ str = if str /= "" then _ { currentFr = Just $ Text str } else _ { currentFr = Nothing }

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

instance decodeJsonText :: A.DecodeJson (Text a) where
  decodeJson json = Text <$> A.decodeJson json

instance encodeJsonText :: A.EncodeJson (Text a) where
  encodeJson (Text str) = A.encodeJson str

instance decodeJsonLocStr :: A.DecodeJson LocStr where
  decodeJson json = do
    obj <- A.decodeJson json
    en_ <- obj .: "en"
    fr_ <- obj .:? "fr"
    ja_ <- obj .:? "ja"
    pure $ LocStr { en: en_, fr: fr_, ja: ja_ }

instance encodeJsonLocStrin :: A.EncodeJson LocStr where
  encodeJson (LocStr loc) =
    "en" := loc.en
      ~> "ja"
      :=? loc.ja
      ~>? "fr"
      :=? loc.fr
      ~>? A.jsonEmptyObject

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    H.liftEffect $ log "done"
    runUI editor unit body

editor :: forall q o m i. MonadEffect m => MonadAff m => H.Component HH.HTML q i o m
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
    , sourceFile: ""
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    let
      ks = keys state.dictionary

      ksOptions = map (\k -> HH.option [ HP.selected (maybe false (k == _) state.currentKey) ] [ HH.text k ]) ks

      lngInput :: forall a. UpdateState a String => FromState a String => String -> String -> LProxy a -> Int -> H.ComponentHTML Action ChildSlots m
      lngInput id title lang n =
        HH.div [ HP.classes [ BS.formGroup, BS.row ] ]
          [ HH.div [ HP.class_ BS.col6 ]
              [ HH.label [ HP.for id ] [ HH.text title ]
              , HH.textarea [ HP.id_ id, HP.class_ BS.formControl, HE.onValueInput (\str -> Just (TextChanged $ updateState lang str)), HP.value $ fromState lang state ]
              ]
          , HH.div
              [ HP.class_ BS.col6 ]
              [ HH.slot _marked n Marked.component { text: fromState lang state, id: id } absurd
              ]
          ]

      editionButtons = case state.currentKey of
        Nothing -> []
        Just key ->
          if key /= "" then
            [ HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary ], HE.onClick $ preventDefault $ Save key ] [ HH.text "Save" ]
            ]
              <> if (member key state.dictionary) then
                  [ HH.button [ HP.classes [ BS.btn, BS.btnOutlineDanger ], HE.onClick $ preventDefault $ Delete key ] [ HH.text "Delete" ]
                  ]
                else
                  []
          else
            []

      newKey =
        HH.div [ HP.classes [ BS.formGroup ] ]
          ( [ HH.label [ HP.for "newkey" ] [ HH.text "New Key" ]
            , HH.input
                [ HP.class_ BS.formControl
                , HP.type_ HP.InputText
                , HP.id_ "newkey"
                , HE.onValueInput $ Just <<< KeyChanged
                , HP.value $ fromMaybe "" state.currentKey
                ]
            ]
              <> editionButtons
          )

      keySelection =
        HH.div [ HP.classes [ BS.formGroup ] ]
          [ HH.label [ HP.for "keyselection" ] [ HH.text "Keys" ]
          , HH.select
              [ HE.onValueChange $ Just <<< KeyChanged
              , HP.id_ "keyselection"
              , HP.class_ BS.formControl
              , HP.value $ maybe "" (\v -> if v `elem` ks then v else "") state.currentKey
              ]
              ksOptions
          ]

      inputs = case state.currentKey of
        Nothing -> []
        Just currKey ->
          [ lngInput "en-input" "English" en 0
          , lngInput "fr-input" "French" fr 1
          , lngInput "ja-input" "Japanese" ja 2
          ]

      jsonFile =
        HH.form_
          [ HH.div [ HP.classes [ BS.formGroup, BS.row ] ]
              [ HH.div [ HP.class_ BS.col12 ]
                  [ HH.label [ HP.for "targeturl" ] [ HH.text "JSON" ]
                  , HH.textarea [ HE.onValueChange $ Just <<< SourceFile, HP.id_ "targetUrl", HP.class_ BS.formControl, HP.value state.sourceFile ]
                  ]
              ]
          , HH.div [ HP.classes [ BS.formGroup, BS.row ] ]
              [ HH.div [ HP.class_ BS.col12 ]
                  [ HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary ], HE.onClick $ preventDefault $ LoadFile ] [ HH.text "Load" ]
                  , HH.button [ HP.classes [ BS.btn, BS.btnOutlinePrimary ], HE.onClick $ preventDefault $ GenerateFile ] [ HH.text "Generate" ]
                  ]
              ]
          ]
    in
      HH.div [ HP.class_ BS.container ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.class_ BS.col12 ]
                [ jsonFile
                , HH.form_
                    ( [ keySelection
                      , newKey
                      ]
                        <> inputs
                    )
                ]
            ]
        ]

  handleAction :: forall output. Action -> H.HalogenM State Action ChildSlots output m Unit
  handleAction = case _ of
    TextChanged change -> do
      H.modify_ change
    KeyChanged key -> do
      dic <- H.gets _.dictionary
      H.modify_ _ { currentKey = Just key }
      case lookup key dic of
        Just (LocStr loc) -> H.modify_ $ updateState en loc.en >>> updateState fr loc.fr >>> updateState ja loc.ja
        Nothing -> H.modify_ $ updateState en (mempty :: Text En) >>> updateState fr (Nothing :: Maybe (Text Fr)) >>> updateState ja (Nothing :: Maybe (Text Jp))
    Save key -> do
      { dictionary, currentEn, currentFr, currentJp } <- H.get
      let
        locstr = LocStr { en: currentEn, fr: currentFr, ja: currentJp }

        newDic = insert key locstr dictionary
      H.modify_ _ { dictionary = newDic }
    Delete key -> do
      dic <- H.gets _.dictionary
      H.modify_ _ { dictionary = delete key dic, currentKey = Nothing, currentEn = mempty :: Text En, currentFr = Nothing, currentJp = Nothing }
    PreventDefault event act -> defaultPrevented event act handleAction
    SourceFile url -> H.modify_ _ { sourceFile = url }
    LoadFile -> do
      source <- H.gets _.sourceFile
      case A.jsonParser source >>= A.decodeJson of
        Left error -> H.liftEffect $ log $ "File loading failed: " <> error
        Right result -> H.modify_ _ { dictionary = result }
    GenerateFile -> do
      dic <- H.gets _.dictionary
      handleAction $ SourceFile $ A.encodeJson dic # A.stringify

  defaultPrevented ::
    forall s a c o1 m1.
    MonadEffect m1 =>
    W.Event -> Maybe a -> (a -> H.HalogenM s a c o1 m1 Unit) -> H.HalogenM s a c o1 m1 Unit
  defaultPrevented event action handle = do
    H.liftEffect $ W.preventDefault event
    maybe (pure unit) handle action

  preventDefault action event = Just $ PreventDefault (toEvent event) $ Just action
