module Main
  ( main
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.Codec (Codec')
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
-- import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Compat (EffectFn1, EffectFn2, EffectFnAff)
import Effect.Aff.Compat as Aff.Compat
import Effect.Aff.Compat as Effect
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as Halogen
import Halogen.Aff as Halogen.Aff
import Halogen.HTML (HTML)
import Halogen.HTML as HTML
import Halogen.HTML.Properties as Properties
import Halogen.VDom.Driver as Driver
import JSON as JSON
import JSON.Object as JSON.Object
import Web.HTML as Web.HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  location <- Window.location =<< Web.HTML.window
  path <- Location.pathname location
  query <- Location.search location
  Halogen.Aff.runHalogenAff do
    body <- Halogen.Aff.awaitBody
    let
      route = Codec.decode routeCodec path
    halogenIO <- Driver.runUI component { route, query } body
    Halogen.liftEffect do
      let
        onPathChange str = Aff.launchAff_ do
          _response <- halogenIO.query (Halogen.mkTell (OnPathChange str))
          pure unit
        onQueryChange str = Aff.launchAff_ do
          _response <- halogenIO.query (Halogen.mkTell (OnQueryChange str))
          pure unit
      setupRouting onPathChange onQueryChange

data Action = Fetch String

type State =
  { response :: Maybe (Either Problem (Array DogBreed))
  , route :: Maybe Route
  , query :: String
  }

type Input = { route :: Maybe Route, query :: String }

data Query a
  = OnPathChange String a
  | OnQueryChange String a

type DogBreed = { breed :: String, subBreed :: Maybe String }

data Problem
  = AffError Aff.Error
  | ParseError String

data Route
  = Home
  | SelectedBreed DogBreed

routeCodec :: Codec' Maybe String Route
routeCodec = Codec.codec'
  ( \path -> case String.split (Pattern "/") path of
      [ "" ] -> Just (Home)
      [ "", breed ] -> Just (SelectedBreed { breed, subBreed: Nothing })
      [ "", breed, subBreed ] -> Just (SelectedBreed { breed, subBreed: Just subBreed })
      _ -> Nothing
  )
  ( case _ of
      Home -> "/"
      SelectedBreed { breed, subBreed } -> case subBreed of
        Nothing -> "/" <> breed
        Just subBreed' -> "/" <> breed <> "/" <> subBreed'
  )

foreign import _fetch :: String -> EffectFnAff String
foreign import _setupRouting
  :: EffectFn2
       (EffectFn1 String Unit)
       (EffectFn1 String Unit)
       (Effect Unit)

fetch :: String -> Aff String
fetch url = Aff.Compat.fromEffectFnAff (_fetch url)

setupRouting
  :: (String -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect (Effect Unit)
setupRouting onRouteChange onQueryChange = do
  Aff.Compat.runEffectFn2 _setupRouting (Effect.mkEffectFn1 onRouteChange) (Effect.mkEffectFn1 onQueryChange)

renderLink :: forall w i. DogBreed -> HTML w i
renderLink dogBreed =
  HTML.a [ Properties.href (Codec.encode routeCodec (SelectedBreed dogBreed)) ]
    [ case dogBreed.subBreed of
        Nothing -> HTML.text dogBreed.breed
        Just subBreed -> HTML.text (dogBreed.breed <> "-" <> subBreed)
    ]

component :: forall output. Component Query Input output Aff
component =
  Halogen.mkComponent
    { initialState
    , render
    , eval
    }
  where
  eval =
    Halogen.mkEval
      ( Halogen.defaultEval
          { handleAction = handleAction
          , handleQuery = handleQuery
          , initialize = Just (Fetch "https://dog.ceo/api/breeds/list/all")
          }
      )

  initialState :: Input -> State
  initialState { route, query } =
    { response: Nothing
    , route
    , query
    }

  render :: State -> ComponentHTML Action () Aff
  render state =
    HTML.section_
      [ HTML.h1_ [ HTML.text "Dogs!" ]
      , HTML.div_
          [ HTML.text case state.route of
              Just (SelectedBreed dogBreed) -> show dogBreed
              Just Home -> "Home"
              Nothing -> "Not found!"
          ]
      , case state.response of
          Nothing -> HTML.text ""
          Just (Left (AffError affError)) -> HTML.text (show affError)
          Just (Left (ParseError parseError)) -> HTML.text parseError
          Just (Right dogs) -> HTML.ul_
            ( dogs <#> \dog ->
                HTML.li_ [ renderLink dog ]
            )
      ]

  handleAction :: Action -> HalogenM State Action () output Aff Unit
  handleAction = case _ of
    Fetch url -> do
      response <- Halogen.liftAff do
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
      Halogen.modify_ \state -> state { response = Just dogs }

  handleQuery :: forall a. Query a -> HalogenM State Action () output Aff (Maybe a)
  handleQuery = case _ of
    OnPathChange str a -> do
      let route = Codec.decode routeCodec str
      Halogen.modify_ \state -> state { route = route }
      pure (Just a)
    OnQueryChange str a -> do
      Halogen.modify_ \state -> state { query = str }
      pure (Just a)
