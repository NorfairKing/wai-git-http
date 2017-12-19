{-# LANGUAGE OverloadedStrings #-}

module GitSpec
    ( spec
    ) where

import Test.Hspec

import Control.Exception

import System.Directory
import System.Exit
import System.Process

import Network.Wai
import Network.Wai.Handler.Warp

import Network.Wai.Application.CGI.Git

spec :: Spec
spec = do
    let dir = "/tmp/git-http-backend-test"
    it "works when you try to clone it" $
        withApp $ \port -> do
            removePathForcibly dir
            createDirectoryIfMissing True dir
            mapM_ (callIn dir) ["git clone http://localhost:" ++ show port]
    it "works when you set a branch upstream" $
        withApp $ \port -> do
            removePathForcibly dir
            createDirectoryIfMissing True dir
            mapM_
                (callIn dir)
                [ "git init"
                , "git remote add testing http://localhost:" ++ show port
                , "touch README.md"
                , "git add README.md"
                , "git commit -m 'initial commit'"
                , "git push --set-upstream testing master"
                ]

withApp :: (Port -> IO ()) -> IO ()
withApp = testWithApplication makeApp

makeApp :: IO Application
makeApp = do
    let dir = "/tmp/git-data"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    callIn dir "git init --bare"
    callIn dir "git config http.receivepack true"
    callIn dir "git config receive.denyCurrentBranch updateInstead"
    pure $ cgiGitBackend dir

callIn :: FilePath -> String -> IO ()
callIn dir cmd = do
    let cp = (shell cmd) {cwd = Just dir}
    (_, _, _, ph) <- createProcess cp
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwIO ec
