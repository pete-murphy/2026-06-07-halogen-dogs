module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as Aff.Compat
import Effect.Class.Console as Console
import Halogen (Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import JSON as JSON
import JSON.Array as JSON.Array
import JSON.Object as JSON.Object

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Fetch String

type Input = Unit
type State =
  { response :: Maybe (Either Problem (Array DogBreed))
  }

type DogBreed = { breed :: String, subBreed :: Maybe String }

data Problem
  = AffError Aff.Error
  | ParseError String

foreign import _fetch :: String -> EffectFnAff String

fetch :: String -> Aff String
fetch url = Aff.Compat.fromEffectFnAff (_fetch url)

component :: forall q o. Component q Input o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval
    }
  where
  eval =
    H.mkEval
      ( H.defaultEval
          { handleAction = handleAction
          , initialize = Just (Fetch "https://dog.ceo/api/breeds/list/all")
          }
      )

  initialState :: Input -> State
  initialState _ =
    { response: Nothing
    }

  render :: State -> H.ComponentHTML Action () Aff
  render state =
    HH.section_
      [ HH.h1_ [ HH.text "Dogs!" ]
      , case Debug.spy "state.response" state.response of
          Nothing -> HH.text ""
          Just (Left (AffError affError)) -> HH.text (show affError)
          Just (Left (ParseError parseError)) -> HH.text parseError
          Just (Right dogs) -> HH.text (show dogs)
      ]

  handleAction :: Action -> H.HalogenM State Action () o Aff Unit
  handleAction = case _ of
    Fetch url -> do
      response <- H.liftAff do
        Aff.attempt (fetch url)
          <#> lmap AffError
      let
        dogs = do
          str <- response
          json <- JSON.parse str # lmap ParseError
          obj <- JSON.toJObject json
            >>= JSON.Object.lookup "message"
            >>= JSON.toJObject
            # Either.note (ParseError "Expected an object with a \"message\" field.")
          parsed :: Array _ <- for (JSON.Object.toUnfoldable obj) \(Tuple breed value) -> do
            subBreeds <-
              (JSON.toArray value >>= traverse JSON.toString)
                <|> (JSON.toNull value $> [])
                # Either.note (ParseError "Expected null or array of strings")
            pure ({ breed, subBreed: _ } <$> (Nothing : map Just subBreeds))
          pure (join parsed)
      H.modify_ \state -> state { response = Just dogs }

