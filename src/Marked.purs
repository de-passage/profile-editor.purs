module Marked (component, Input(..), Slot(..)) where

import Prelude
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Const (Const)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Gen as DSG
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

foreign import marked :: String -> Effect String

foreign import markedByElementId :: Fn2 String String (Effect Unit)

foreign import setHTML :: HTMLElement -> String -> Effect Unit

type Input
  = { text :: String, id :: String }

data Action
  = Init
  | ChangeValue String

type State
  = { text :: String
    , contextName :: String
    }

type Slot
  = H.Slot (Const Void) Void

component :: forall q o m. MonadEffect m => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              , receive = Just <<< ChangeValue <<< _.text
              }
    }
  where
  initialState :: Input -> State
  initialState { text, id } = { text, contextName: "halogen-marked-renderer-" <> id }

  render :: forall i. State -> HH.HTML i Action
  render s = HH.div [ HP.ref (H.RefLabel s.contextName) ] []

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      { text } <- H.get
      handleAction (ChangeValue text)
    ChangeValue value -> do
      s <- H.get
      elem <- H.getHTMLElementRef (H.RefLabel s.contextName)
      H.liftEffect do
        md <- marked value
        elem
          # maybe (pure unit) \e ->
              setHTML e md
