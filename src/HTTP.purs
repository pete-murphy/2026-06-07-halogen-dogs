module HTTP where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as Aff.Compat
import JSON (JSON)
import JSON as JSON

foreign import _get :: String -> EffectFnAff String

get :: forall a. String -> (JSON -> Either String a) -> Aff (Either Error a)
get url parse = do
  response <- Aff.attempt (Aff.Compat.fromEffectFnAff (_get url))
  let
    parsed = do
      str <- response # lmap AffError
      json <- JSON.parse str # lmap ParseError
      parse json # lmap ParseError
  pure parsed

data Error
  = AffError Aff.Error
  | ParseError String

instance Show Error where
  show = case _ of
    AffError err -> "AffError (" <> show err <> ")"
    ParseError err -> "ParseError (" <> err <> ")"
