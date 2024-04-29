{-# LANGUAGE LambdaCase #-}

module ImportStylePlugin.Yaml where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor (($>))
import Data.String (IsString (..))
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import qualified GHC.Plugins as Ghc
import ImportStylePlugin (importPlugin)
import ImportStylePlugin.Compat (report)
import ImportStylePlugin.Config (Severity (..))

plugin :: Ghc.Plugin
plugin =
  Ghc.defaultPlugin
    { Ghc.typeCheckResultAction = \opts _ a -> do
        case opts of
          [filepath] -> do
            liftIO (decodeFileEither filepath) >>= \case
              Right style -> importPlugin style a
              Left err -> report Warning (fromString $ prettyPrintParseException err) Nothing $> a
          _ -> pure a
    }
