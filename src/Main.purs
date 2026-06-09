module Main
  ( main
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as Array
import Data.Codec (Codec')
import Data.Codec as Codec
import Data.Either (Either)
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn1)
import Effect.Aff.Compat as Aff.Compat
import Effect.Aff.Compat as Effect
import HTTP as HTTP
import HTTP.Loadable (Loadable)
import HTTP.Loadable as Loadable
import Halogen (AttrName(..), Component, ComponentHTML, HalogenM)
import Halogen as Halogen
import Halogen.Aff as Halogen.Aff
import Halogen.HTML (HTML)
import Halogen.HTML as HTML
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as Properties
import Halogen.VDom.Driver as Driver
import JSON (JSON)
import JSON as JSON
import JSON.Array as JSON.Array
import JSON.Object as JSON.Object
import Promise (Promise)
import Promise.Aff as Promise.Aff
import URL (URL)
import URL as URL
import Web.HTML as Web.HTML
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  url <- URL.fromLocation =<< Window.location =<< Web.HTML.window
  Halogen.Aff.runHalogenAff do
    body <- Halogen.Aff.awaitBody
    halogenIO <- Driver.runUI app url body
    Halogen.liftEffect do
      let
        onNavigate url' = void do
          halogenIO.query (Halogen.mkTell (OnNavigate url'))
      setupRouting onNavigate

data Action = Initialize

type State =
  { url :: URL
  , shared :: Shared
  , page :: Page
  }

type Shared =
  { breeds :: Loadable (Array DogBreed) }

-- | Page state is derivable from the Route (which is derived from `state.url`)
-- | but can sometimes lag behind, e.g., when transitioning a route requires
-- | loading some resource.
data Page
  = Page'Home
  | Page'Breed { images :: Loadable (Array String) }
  | Page'Image
  | Page'NotFound

type Input = URL

data Query a = OnNavigate URL a

type DogBreed = { parentBreed :: String, subBreed :: Maybe String }

data Route
  = Route'Home
  | Route'Breed DogBreed
  | Route'Image DogBreed String

routeCodec :: Codec' Maybe String Route
routeCodec = Codec.codec'
  ( \path -> case String.split (Pattern "/") path # Array.drop 1 of
      [ "" ] -> Just Route'Home
      [ parentBreed ] -> Just (Route'Breed { parentBreed, subBreed: Nothing })
      [ parentBreed, subBreed ] -> Just (Route'Breed { parentBreed, subBreed: Just subBreed })
      [ parentBreed, "image", image ] -> Just (Route'Image { parentBreed, subBreed: Nothing } image)
      [ parentBreed, subBreed, "image", image ] -> Just (Route'Image { parentBreed, subBreed: Just subBreed } image)
      _ -> Nothing
  )
  ( case _ of
      Route'Home -> "/"
      Route'Breed { parentBreed, subBreed } -> case subBreed of
        Nothing -> "/" <> parentBreed
        Just subBreed' -> "/" <> parentBreed <> "/" <> subBreed'
      Route'Image { parentBreed, subBreed } image -> (_ <> "/image/" <> image) case subBreed of
        Nothing -> "/" <> parentBreed
        Just subBreed' -> "/" <> parentBreed <> "/" <> subBreed'
  )

parseRoute :: URL -> Maybe Route
parseRoute url =
  Codec.decode routeCodec (URL.pathname url)

pageFromMaybeRoute :: Maybe Route -> Page
pageFromMaybeRoute = case _ of
  Nothing -> Page'NotFound
  Just Route'Home -> Page'Home
  Just (Route'Breed _) -> Page'Breed { images: Loadable.notAsked }
  Just (Route'Image _ _) -> Page'Image

foreign import _setupRouting
  :: EffectFn1
       (EffectFn1 URL (Promise Unit))
       (Effect Unit)

setupRouting
  :: (URL -> Aff Unit)
  -> Effect (Effect Unit)
setupRouting onNavigate = do
  Aff.Compat.runEffectFn1 _setupRouting
    (Effect.mkEffectFn1 (Promise.Aff.fromAff <<< onNavigate))

renderLink :: forall w i. DogBreed -> HTML w i
renderLink breed@{ parentBreed, subBreed } =
  HTML.a [ routeHref (Route'Breed breed) ]
    [ case subBreed of
        Nothing -> HTML.text parentBreed
        Just subBreed' -> HTML.text (parentBreed <> "-" <> subBreed')
    ]

routeHref :: forall r i. Route -> IProp (href :: String | r) i
routeHref route = Properties.href (Codec.encode routeCodec route)

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
    , shared: { breeds: Loadable.loading }
    , page: pageFromMaybeRoute (parseRoute url)
    }

  render :: State -> ComponentHTML Action () Aff
  render state =
    HTML.main_
      [ HTML.header_
          [ HTML.h1_ [ HTML.a [ routeHref Route'Home ] [ HTML.text "Dogs!" ] ] ]
      , HTML.div_
          [ HTML.text case parseRoute state.url of
              Just Route'Home -> "Home"
              Just (Route'Image _ image) -> image
              Just (Route'Breed dogBreed) -> show dogBreed
              Nothing -> "Not found!"
          ]
      , case Loadable.value state.shared.breeds of
          Loadable.Empty -> HTML.text ""
          Loadable.Failure err -> HTML.text (show err)
          Loadable.Success dogs -> HTML.ul_
            ( Array.take 20 dogs <#> \dogBreed ->
                HTML.li_ [ renderLink dogBreed ]
            )
      , case state.page of
          Page'Home -> HTML.text ""
          Page'Breed { images } -> case Loadable.value images of
            Loadable.Empty -> HTML.text if Loadable.isLoading images then "Loading..." else "Not loading (this is unexpected!)"
            Loadable.Failure err -> HTML.text (show err)
            Loadable.Success images' -> HTML.ul_
              (images' <#> \image -> HTML.li_ [ renderImage image ])
          Page'Image -> HTML.text "Image"
          Page'NotFound -> HTML.text "Not found!"
      ]

  handleAction :: Action -> HalogenM State Action () output Aff Unit
  handleAction = case _ of
    Initialize -> do
      dogs <- Halogen.liftAff do
        HTTP.get "https://dog.ceo/api/breeds/list/all"
          ( withMessage \message -> do
              obj <- JSON.toJObject message # Either.note "Expected an object"
              parsed :: Array _ <- for (JSON.Object.toUnfoldable obj) \(Tuple parentBreed value) -> do
                subBreeds <-
                  (JSON.toArray value >>= traverse JSON.toString)
                    <|> (JSON.toNull value $> [])
                    # Either.note "Expected null or array of strings"
                pure ({ parentBreed, subBreed: _ } <$> (Nothing : map Just subBreeds))
              pure (join parsed)
          )
      url <- Halogen.gets _.url
      page <- Halogen.liftAff (loadPageForURL url)
      Halogen.modify_ \state -> state
        { page = page
        , shared = { breeds: Loadable.fromEither dogs }
        }

  handleQuery :: forall a. Query a -> HalogenM State Action () output Aff (Maybe a)
  handleQuery = case _ of
    OnNavigate url next -> do
      current <- Halogen.get
      when (current.url /= url) do
        Halogen.modify_ \state -> state { url = url }
        page <- Halogen.liftAff (loadPageForURL url)
        Halogen.modify_ \state -> state { page = page }
      pure (Just next)

loadPageForURL :: URL -> Aff Page
loadPageForURL url =
  case parseRoute url of
    Nothing ->
      pure Page'NotFound
    Just Route'Home ->
      pure Page'Home
    Just (Route'Breed breed) -> do
      let
        path = case breed.subBreed of
          Just subBreed -> breed.parentBreed <> "/" <> subBreed
          Nothing -> breed.parentBreed
        url' = "https://dog.ceo/api/breed/" <> path <> "/images"

      images <- Halogen.liftAff do
        HTTP.get url'
          ( withMessage \message -> do
              JSON.toJArray message >>= JSON.Array.toArray >>> traverse JSON.toString
                # Either.note "Expected an array of strings"
          )
      pure (Page'Breed { images: Loadable.fromEither images })
    Just (Route'Image _ _) ->
      pure Page'Image

withMessage :: forall a. (JSON -> Either String a) -> JSON -> Either String a
withMessage k json = do
  obj <- JSON.toJObject json
    >>= JSON.Object.lookup "message"
    # Either.note "Expected an object with a \"message\" field."
  k obj
