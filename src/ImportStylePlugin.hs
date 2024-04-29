{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module ImportStylePlugin where

import Control.Monad (forM_, unless)
import Data.Functor (($>), (<&>))
import Data.List (intercalate, intersperse)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified GHC.Hs as Ghc
import qualified GHC.Plugins as Ghc
import qualified GHC.Tc.Types as Ghc
import ImportStylePlugin.Compat (getExplicitlyHiddenNames, getExplicitlyImportedNames, report)
import ImportStylePlugin.Config as Config

importPlugin ::
  ImportsStyle ->
  Ghc.TcGblEnv ->
  Ghc.TcM Ghc.TcGblEnv
importPlugin ImportsStyle{..} e@Ghc.TcGblEnv{tcg_rn_imports} = corrections $> e
 where
  mkWord = \case
    Error -> "must"
    Warning -> "should"

  corrections = forM_ tcg_rn_imports \(Ghc.L Ghc.SrcSpanAnn{..} importDecl@Ghc.ImportDecl{..}) ->
    let (Ghc.L _ (Ghc.moduleNameString -> moduleName)) = ideclName
     in do
          case qualificationStyle of
            _ | ideclQualified == Ghc.NotQualified -> pure ()
            Just Pre | ideclQualified /= Ghc.QualifiedPre -> report Error "Use prefix qualification" (Just locA)
            Just Post | ideclQualified /= Ghc.QualifiedPost -> report Error "Use postfix qualification" (Just locA)
            _ -> pure ()

          case Map.lookup (Config.ModuleName moduleName) bannedModules of
            Just Ban{..} -> report severity (fromString why) (Just locA)
            Nothing -> maybe
              do pure ()
              do
                \ImportRules{..} -> unless (any (isImportValid importDecl) rules) do
                  let header = fromString $ "Import " <> mkWord severity <> " satisfy the following rules:"
                  let allStringRules = Ghc.vcat $ (header :) $ intersperse "or" $ map (Ghc.vcat . stringifyRule severity) rules
                  report severity allStringRules (Just locA)
              do Map.lookup (Config.ModuleName moduleName) importRules

  stringifyRule severity ImportRule{..} =
    map (fromString . ("  * " <>)) $
      catMaybes
        [ qualification <&> \case
            Required -> "Module " <> word <> " be qualified"
            Forbidden -> "Module " <> word <> " not be qualified"
        , let common (Set.toList -> allowed) =
                "Alias " <> word <> " be " <> case allowed of
                  [name] -> name
                  _ -> "one of " <> commaSep allowed
           in aliases <&> \case
                Exactly allowed -> if Set.null allowed then "Import " <> word <> " not have alias" else common allowed
                OrOmitted allowed -> common allowed <> " or alias can be omitted"
        , importedNames <&> \case
            BlackList names -> "Module " <> word <> " not import the following names: " <> printNamesList names
            WhiteList names | Set.null names -> "Module" <> word <> " not import names explicitly"
            WhiteList names -> "Module " <> word <> " import only the following names: " <> printNamesList names
        ]
   where
    printNamesList = commaSep . Set.toList
    commaSep = intercalate ", "

    word = mkWord severity

  isImportValid :: Ghc.ImportDecl Ghc.GhcRn -> ImportRule -> Bool
  isImportValid decl@Ghc.ImportDecl{..} ImportRule{..} =
    isQualificationValid
      && areImportedNamesValid
      && isAliasValid
   where
    isQualificationValid = case qualification of
      Just Required -> ideclQualified /= Ghc.NotQualified
      Just Forbidden -> ideclQualified == Ghc.NotQualified
      Nothing -> True

    isAliasValid = case aliases of
      Just (Exactly allowedAliases) -> oneOfAllowed False allowedAliases
      Just (OrOmitted allowedAliases) -> oneOfAllowed True allowedAliases
      Nothing -> True
     where
      oneOfAllowed def allowed = maybe
        do def
        do \(Ghc.L _ name) -> Ghc.moduleNameString name `Set.member` allowed
        do ideclAs

    areImportedNamesValid = case importedNames of
      Just (WhiteList allowedNames) ->
        maybe False (all (`Set.member` allowedNames)) (getExplicitlyImportedNames decl)
      Just (BlackList forbiddenNames) ->
        maybe True (Set.null . (forbiddenNames `Set.difference`) . Set.fromList) (getExplicitlyHiddenNames decl)
          && maybe False (not . any (`Set.member` forbiddenNames)) (getExplicitlyImportedNames decl)
      Nothing -> True
