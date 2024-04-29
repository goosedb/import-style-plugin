{- FOURMOLU_DISABLE -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module ImportStylePlugin.Compat where

#if __GLASGOW_HASKELL__ >= 908 && __GLASGOW_HASKELL__ < 910
import qualified GHC as Ghc
import qualified GHC.Utils.Outputable as Ghc
import qualified GHC.Types.Error as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.Tc.Errors.Types as Ghc
import qualified GHC.Unit.Module.Warnings as Ghc
#endif

#if __GLASGOW_HASKELL__ >= 906 && __GLASGOW_HASKELL__ < 908
import qualified GHC as Ghc
import qualified GHC.Types.Error as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.Tc.Errors.Types as Ghc
import qualified GHC.Utils.Outputable as Ghc
#endif

#if __GLASGOW_HASKELL__ >= 904 && __GLASGOW_HASKELL__ < 906
import qualified GHC as Ghc
import qualified GHC.Tc.Types as Ghc
import qualified GHC.Utils.Error as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.Tc.Errors.Types as Ghc
import qualified GHC.Types.Error as Ghc
import qualified GHC.Driver.Ppr as Ghc
#endif

#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 904
import qualified GHC as Ghc
import qualified GHC.Utils.Outputable as Ghc
import qualified GHC.Tc.Utils.Monad as Ghc
import qualified GHC.Driver.Flags as Ghc
#endif

import ImportStylePlugin.Config as Cfg

getRawNames :: [Ghc.GenLocated l (Ghc.IE Ghc.GhcRn)] -> [String]
getRawNames names =
  [ Ghc.showPprUnsafe  n
  | Ghc.L
      _
      n <-
      names
  ] 

getExplicitlyImportedNames :: Ghc.ImportDecl Ghc.GhcRn -> Maybe [String]
getExplicitlyImportedNames Ghc.ImportDecl {..} =
  case 
#if __GLASGOW_HASKELL__ >= 906 
    ideclImportList 
#else 
    ideclHiding
#endif
  of
    Just 
      (
#if __GLASGOW_HASKELL__ >= 906 
        Ghc.Exactly
#else 
        False
#endif
      , Ghc.L _ names
      ) -> Just (getRawNames names)
    _ -> Nothing

getExplicitlyHiddenNames :: Ghc.ImportDecl Ghc.GhcRn -> Maybe [String]
getExplicitlyHiddenNames Ghc.ImportDecl {..} =
  case 
#if __GLASGOW_HASKELL__ >= 906 
    ideclImportList 
#else 
    ideclHiding
#endif
  of
    Just 
      ( 
#if __GLASGOW_HASKELL__ >= 906 
        Ghc.EverythingBut
#else 
        True
#endif
      , Ghc.L _ names) -> Just (getRawNames names)
    _ -> Nothing



#if __GLASGOW_HASKELL__ >= 908 && __GLASGOW_HASKELL__ < 910
report :: Severity -> Ghc.SDoc -> Maybe Ghc.SrcSpan -> Ghc.TcRn ()
report severity msg loc =
  maybe Ghc.addDiagnostic Ghc.addDiagnosticAt loc
    . Ghc.TcRnUnknownMessage
    . Ghc.mkSimpleUnknownDiagnostic
    $ Ghc.DiagnosticMessage
      { Ghc.diagMessage = Ghc.mkSimpleDecorated msg
      , Ghc.diagReason = case severity of
          Error -> Ghc.ErrorWithoutFlag
          Warning -> Ghc.WarningWithCategory (Ghc.WarningCategory "x-import-style")
      , Ghc.diagHints = []
      }
#endif

#if __GLASGOW_HASKELL__ >= 906 && __GLASGOW_HASKELL__ < 908
report :: Severity -> Ghc.SDoc -> Maybe Ghc.SrcSpan -> Ghc.TcRn ()
report severity msg loc =
  maybe Ghc.addDiagnostic Ghc.addDiagnosticAt loc
    . Ghc.TcRnUnknownMessage
    . Ghc.UnknownDiagnostic
    $ Ghc.DiagnosticMessage
      { Ghc.diagMessage = Ghc.mkSimpleDecorated msg
      , Ghc.diagReason = case severity of
          Error -> Ghc.ErrorWithoutFlag
          Warning -> Ghc.WarningWithoutFlag 
      , Ghc.diagHints = []
      }
#endif

    
#if __GLASGOW_HASKELL__ >= 904 && __GLASGOW_HASKELL__ < 906
report :: Severity -> Ghc.SDoc -> Maybe Ghc.SrcSpan -> Ghc.TcRn ()
report severity msg loc = 
  maybe Ghc.addDiagnostic Ghc.addDiagnosticAt loc
    . Ghc.TcRnUnknownMessage
    $ Ghc.DiagnosticMessage
      { Ghc.diagMessage = Ghc.mkSimpleDecorated msg
      , Ghc.diagReason = case severity of
          Error -> Ghc.ErrorWithoutFlag
          Warning -> Ghc.WarningWithoutFlag
      , Ghc.diagHints = []
      }
#endif


#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 904
report :: Severity -> Ghc.SDoc -> Maybe Ghc.SrcSpan -> Ghc.TcRn ()
report severity msg loc = 
  maybe 
    do case severity of
        Error -> Ghc.addErr msg
        Warning -> Ghc.addWarn Ghc.NoReason msg
    do \l -> case severity of
        Error -> Ghc.addErrAt l msg
        Warning -> Ghc.addWarnAt Ghc.NoReason l msg
    do loc 
#endif
