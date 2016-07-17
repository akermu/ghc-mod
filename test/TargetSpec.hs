{-# LANGUAGE OverloadedStrings #-}
module TargetSpec where

import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.LightGhc
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Logger

import Test.Hspec

import TestUtils
import Dir

import GHC
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Process

import qualified DynFlags      as GHC
import qualified GHC           as GHC
import qualified Module        as GHC
import qualified Outputable    as GHC

spec :: Spec
spec = do
    describe "runLightGhc" $ do
        it "works at all" $ do
            withLightHscEnv [] $ \env ->
              runLightGhc env (return ()) `shouldReturn` ()

        it "has modules in scope" $ do
            (withLightHscEnv [] $ \env ->
              runLightGhc env $ do
               dflags <- getSessionDynFlags
               let i = intersect (listVisibleModuleNames dflags)
                                 ["Control.Applicative", "Control.Arrow"
                                 ,"Control.Exception", "GHC.Exts", "GHC.Float"]
               liftIO $ i `shouldSatisfy` not . null) :: IO ()

        it "can get module info" $ do
            (withLightHscEnv [] $ \env ->
              runLightGhc env $ do
                mdl <- findModule "Data.List" Nothing
                mmi <- getModuleInfo mdl
                liftIO $ isJust mmi `shouldBe` True) :: IO ()


    describe "resolveModule" $ do
        it "Works when a module given as path uses CPP" $ do
            dir <- getCurrentDirectory
            let srcDirs = [dir </> "test/data/target/src"]
            x <- withLightHscEnv [] $ \env -> runD $ do
                resolveModule env srcDirs (Left $ dir </> "test/data/target/Cpp.hs")
            liftIO $ x `shouldBe` Just (ModulePath "Cpp" $ dir </> "test/data/target/Cpp.hs")

    describe "runGmlT" $ do
        it "main module import packageKey should not be 'main@main'" $ do
            withDirectory_ "test/data/cabal-packagekey" $ do
                res <- runD $ checkPackageKeys
                print res
                res `shouldBe` []

checkPackageKeys :: IOish m => GhcModT m [String]
checkPackageKeys =
    runGmlTWith
      [Left "./src/Main.hs"]
      return
      id
      action
  where
    action = do
      graph  <- GHC.getModuleGraph
      let modSum = head graph
      setGhcContext modSum
      gnames <- GHC.getNamesInScope
      liftIO $ putStrLn $ showGhcQual $ map GHC.nameModule gnames
      return $ (filter ("main@main" `isPrefixOf`) (map (showGhcQual . GHC.nameModule) gnames))

showGhcQual :: (GHC.Outputable a) => a -> String
showGhcQual x = GHC.showSDocForUser GHC.unsafeGlobalDynFlags GHC.alwaysQualify $ GHC.ppr x
setGhcContext :: GHC.GhcMonad m => GHC.ModSummary -> m ()

setGhcContext modSum = GHC.setContext [GHC.IIModule (GHC.moduleName $ GHC.ms_mod modSum)]
