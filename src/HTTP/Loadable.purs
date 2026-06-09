module HTTP.Loadable
  ( Loadable
  , notAsked
  , loading
  , succeed
  , fail
  , isLoading
  , value
  , fromEither
  , Value(..)
  ) where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Writer (Writer, WriterT(..), writer)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Tuple (Tuple(..))
import HTTP as HTTP

type Loadable a = ExceptT HTTP.Error (MaybeT (Writer (Disj Boolean))) a

make :: forall a. Value a -> Boolean -> Loadable a
make value' isLoading' = do
  let
    x = case value' of
      Empty -> Nothing
      Failure err -> Just (Left err)
      Success a -> Just (Right a)
  ExceptT (MaybeT (writer (Tuple x (Disj isLoading'))))

data Value a
  = Empty
  | Failure HTTP.Error
  | Success a

notAsked :: forall a. Loadable a
notAsked = make Empty false

loading :: forall a. Loadable a
loading = make Empty true

succeed :: forall a. a -> Loadable a
succeed a = make (Success a) false

fail :: forall a. HTTP.Error -> Loadable a
fail err = make (Failure err) false

isLoading :: forall a. Loadable a -> Boolean
isLoading = case _ of
  ExceptT (MaybeT (WriterT (Identity (Tuple _ (Disj l))))) -> l

value :: forall a. Loadable a -> Value a
value = case _ of
  ExceptT (MaybeT (WriterT (Identity (Tuple Nothing _)))) -> Empty
  ExceptT (MaybeT (WriterT (Identity (Tuple (Just (Left e)) _)))) -> Failure e
  ExceptT (MaybeT (WriterT (Identity (Tuple (Just (Right a)) _)))) -> Success a

fromEither :: forall a. Either HTTP.Error a -> Loadable a
fromEither = case _ of
  Left err -> fail err
  Right a -> succeed a