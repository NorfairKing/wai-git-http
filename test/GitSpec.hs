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
spec =
    it "works" $
    testWithApplication makeApp $ \port -> do
        let dir = "/tmp/git-http-backend-test"
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

makeApp :: IO Application
makeApp = do
    let dir = "/tmp/git-data/"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    callIn dir "git init --bare"
    callIn dir "git config http.receivepack true"
    pure $ cgiGitBackend dir

callIn :: FilePath -> String -> IO ()
callIn dir cmd = do
    let cp = (shell cmd) {cwd = Just dir}
    (_, _, _, ph) <- createProcess cp
    ec <- waitForProcess ph
    case ec of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwIO ec
