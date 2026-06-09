module URL
  ( pathname
  , fromLocation
  , URL
  ) where

import Prelude

import Data.Function.Uncurried (Fn2)
import Data.Function.Uncurried as Function.Uncurried
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Effect.Uncurried
import Web.HTML (Location)

foreign import data URL :: Type
foreign import data URLSearchParams :: Type
foreign import searchParams :: URL -> URLSearchParams
foreign import pathname :: URL -> String
foreign import _fromLocation :: EffectFn1 Location URL
foreign import _eqURL :: Fn2 URL URL Boolean

fromLocation :: Location -> Effect URL
fromLocation = Effect.Uncurried.runEffectFn1 _fromLocation

instance Eq URL where
  eq = Function.Uncurried.runFn2 _eqURL
