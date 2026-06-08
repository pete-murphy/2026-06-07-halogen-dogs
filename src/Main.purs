module Main
  ( main
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec (Codec')
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either as Either
import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as Uncurried
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Compat (EffectFn1, EffectFnAff)
import Effect.Aff.Compat as Aff.Compat
import Effect.Aff.Compat as Effect
import Halogen (AttrName(..), Component, ComponentHTML, HalogenM)
import Halogen as Halogen
import Halogen.Aff as Halogen.Aff
import Halogen.HTML (HTML)
import Halogen.HTML as HTML
import Halogen.HTML.Properties as Properties
import Halogen.VDom.Driver as Driver
import JSON as JSON
import JSON.Array as JSON.Array
import JSON.Object as JSON.Object
import Promise (Promise)
import Promise.Aff as Promise

main :: Effect Unit
main = do
  url <- currentURL
  Halogen.Aff.runHalogenAff do
    body <- Halogen.Aff.awaitBody
    halogenIO <- Driver.runUI app url body
    Halogen.liftEffect do
      let
        onNavigate :: URL -> Aff Unit
        onNavigate url' = void do
          halogenIO.query (Halogen.mkTell (OnNavigate url'))
      setupRouting onNavigate

data Action = Initialize

type State =
  { url :: URL
  , response :: Maybe (Either Problem (Array DogBreed))
  , images :: Maybe (Either Problem (Array String))
  }

type Input = URL

data Query a = OnNavigate URL a

type DogBreed = { breed :: String, subBreed :: Maybe String }

data Problem
  = AffError Aff.Error
  | ParseError String

data Route
  = Home
  | SelectedBreed DogBreed

derive instance Eq Route

routeCodec :: Codec' Maybe String Route
routeCodec = Codec.codec'
  ( \path -> case String.split (Pattern "/") path # Array.drop 1 of
      [ "" ] -> Just Home
      [ breed ] -> Just (SelectedBreed { breed, subBreed: Nothing })
      [ breed, subBreed ] -> Just (SelectedBreed { breed, subBreed: Just subBreed })
      _ -> Nothing
  )
  ( case _ of
      Home -> "/"
      SelectedBreed { breed, subBreed } -> case subBreed of
        Nothing -> "/" <> breed
        Just subBreed' -> "/" <> breed <> "/" <> subBreed'
  )

parseRoute :: URL -> Maybe Route
parseRoute url =
  Codec.decode routeCodec (pathname url)

foreign import _fetch :: String -> EffectFnAff String
foreign import _setupRouting
  :: EffectFn1
       (EffectFn1 URL (Promise Unit))
       (Effect Unit)

fetch :: String -> Aff String
fetch url = Aff.Compat.fromEffectFnAff (_fetch url)

setupRouting
  :: (URL -> Aff Unit)
  -> Effect (Effect Unit)
setupRouting onNavigate = do
  Aff.Compat.runEffectFn1 _setupRouting
    (Effect.mkEffectFn1 (Promise.fromAff <<< onNavigate))

foreign import data URL :: Type
foreign import data URLSearchParams :: Type
foreign import searchParams :: URL -> URLSearchParams
foreign import pathname :: URL -> String
foreign import eqURL :: Fn2 URL URL Boolean
foreign import currentURL :: Effect URL

instance Eq URL where
  eq = Uncurried.runFn2 eqURL

renderLink :: forall w i. DogBreed -> HTML w i
renderLink dogBreed =
  HTML.a [ Properties.href (Codec.encode routeCodec (SelectedBreed dogBreed)) ]
    [ case dogBreed.subBreed of
        Nothing -> HTML.text dogBreed.breed
        Just subBreed -> HTML.text (dogBreed.breed <> "-" <> subBreed)
    ]

renderImage :: forall w i. String -> HTML w i
renderImage image =
  HTML.img
    [ Properties.src image
    , Properties.attr (AttrName "loading") "lazy"
    , Properties.alt "" -- No meaningful alt text
    ]

app :: forall output. Component Query Input output Aff
app =
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
          , initialize = Just Initialize
          }
      )

  initialState :: Input -> State
  initialState url =
    { url
    , response: Nothing
    , images: Nothing
    }

  render :: State -> ComponentHTML Action () Aff
  render state =
    HTML.main_
      [ HTML.header_ [ HTML.h1_ [ HTML.text "Dogs!" ] ]
      , HTML.div_
          [ HTML.text case parseRoute state.url of
              Just (SelectedBreed dogBreed) -> show dogBreed
              Just Home -> "Home"
              Nothing -> "Not found!"
          ]
      , case state.response of
          Nothing -> HTML.text ""
          Just (Left (AffError affError)) -> HTML.text (show affError)
          Just (Left (ParseError parseError)) -> HTML.text parseError
          Just (Right dogs) -> HTML.ul_
            ( Array.take 10 dogs <#> \dog ->
                HTML.li_ [ renderLink dog ]
            )
      , case state.images of
          Nothing -> HTML.text ""
          Just (Left (AffError affError)) -> HTML.text (show affError)
          Just (Left (ParseError parseError)) -> HTML.text parseError
          Just (Right images) -> HTML.ul_
            ( images <#> \image ->
                HTML.li_ [ renderImage image ]
            )
      ]

  handleAction :: Action -> HalogenM State Action () output Aff Unit
  handleAction = case _ of
    Initialize -> do
      response <- Halogen.liftAff do
        Aff.attempt (fetch "https://dog.ceo/api/breeds/list/all")
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

      Halogen.gets _.url >>= fetchImagesForURL
      Halogen.modify_ \state -> state { response = Just dogs }

  handleQuery :: forall a. Query a -> HalogenM State Action () output Aff (Maybe a)
  handleQuery = case _ of
    OnNavigate url next -> do
      current <- Halogen.get
      Halogen.modify_ \state -> state { url = url }
      when (current.url /= url) do
        fetchImagesForURL url
      pure (Just next)

fetchImagesForURL :: forall output. URL -> HalogenM State Action () output Aff Unit
fetchImagesForURL appURL = do
  images <- Halogen.liftAff do
    let
      parsedRoute = parseRoute appURL >>= case _ of
        Home -> Nothing
        other -> Just other
    images <- for parsedRoute \route -> do
      let
        path = Codec.encode routeCodec route
        url = "https://dog.ceo/api/breed" <> path <> "/images"
      response <- Aff.attempt (fetch url)
        <#> lmap AffError
      let
        images = do
          str <- response
          json <- JSON.parse str # lmap ParseError
          arr <- JSON.toJObject json
            >>= JSON.Object.lookup "message"
            >>= JSON.toJArray
            # Either.note (ParseError "Expected an object with a \"message\" field.")
          parsed :: Array _ <- for (JSON.Array.toUnfoldable arr) \image ->
            do
              JSON.toString image
              # Either.note (ParseError "Expected string")
          pure parsed
      pure images
    pure images

  Halogen.modify_ \state -> state { images = images }
