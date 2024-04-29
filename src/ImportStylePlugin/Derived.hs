module ImportStylePlugin.Derived where

import qualified GHC.Plugins as Ghc
import ImportStylePlugin
import ImportStylePlugin.Config

plugin :: ImportsStyle -> Ghc.Plugin
plugin style =
  Ghc.defaultPlugin
    { Ghc.typeCheckResultAction = \_ _ c -> importPlugin style c
    , Ghc.pluginRecompile = Ghc.purePlugin
    }
