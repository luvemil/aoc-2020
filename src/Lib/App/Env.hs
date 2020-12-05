{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env (
    Env (..),
    grab,
) where

import Lib.StaticInfo (StaticInfo)

import Control.Monad.Reader (
    MonadReader,
    ask,
 )
import Data.Kind (Type)

data Env (m :: Type -> Type) = Env
    { sInfo :: StaticInfo
    }

class Has field env where
    obtain :: env -> field

instance Has StaticInfo (Env m) where
    obtain = sInfo

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = fmap (obtain @field) ask
{-# INLINE grab #-}
