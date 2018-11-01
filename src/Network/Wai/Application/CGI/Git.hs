{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Application.CGI.Git
    ( cgiGitBackend
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Control.Exception (SomeException(..), bracket, catch)
import Control.Monad

import System.Environment (lookupEnv)
import System.FilePath
import System.IO
import System.Process

import Network.HTTP.Types
import Network.SockAddr (showSockAddr)
import Network.Wai
import Network.Wai.Conduit

import Network.Wai.Application.CGI.Git.Conduit (parseHeader, toResponseSource)

-- | A git back-end
--
-- The git base dir is the directory for the git repository to serve.  This is
-- `repository` for bare repositories and `repository/.git` for non-bare
-- repositories.
--
-- WARNING: This does not set up any repositories for you, it only serves them
-- you still have to take care of the repositories (and their configuration)
-- behind the scenes.
cgiGitBackend ::
       FilePath -- ^ Git base dir
    -> Application
cgiGitBackend baseDir req respond =
    case parseMethod $ requestMethod req of
        Right GET -> cgiGitBackendApp baseDir False req respond
        Right POST -> cgiGitBackendApp baseDir True req respond
        _ ->
            respond $
            responseLBS
                methodNotAllowed405
                textPlainHeader
                "Method Not Allowed\r\n"

textPlainHeader :: ResponseHeaders
textPlainHeader = [(hContentType, "text/plain")]

cgiGitBackendApp :: FilePath -> Bool -> Application
cgiGitBackendApp baseDir body req respond =
    bracket setup teardown (respond <=< cgi)
  where
    setup = execGitBackendProcess baseDir req
    teardown (rhdl, whdl, pid) = do
        terminateProcess pid -- SIGTERM
        hClose rhdl
        hClose whdl
    cgi (rhdl, whdl, _) = do
        when body $ toCGI whdl req
        hClose whdl -- telling EOF
        fromCGI rhdl

execGitBackendProcess ::
       FilePath -> Request -> IO (Handle, Handle, ProcessHandle)
execGitBackendProcess baseDir req = do
    let naddr = showSockAddr . remoteHost $ req
    epath <- lookupEnv "PATH"
    (Just whdl, Just rhdl, _, pid) <- createProcess $ proSpec naddr epath
    hSetEncoding rhdl latin1
    hSetEncoding whdl latin1
    return (rhdl, whdl, pid)
  where
    proSpec naddr epath =
        CreateProcess
            { cmdspec = RawCommand "/usr/bin/git" ["http-backend"]
            , cwd = Nothing
            , env =
                  Just $
                  makeEnv
                      baseDir
                      req
                      naddr
                      "git http-backend"
                      (show $ rawPathInfo req)
                      "git http-backend"
                      epath
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = True
            , create_group = True
            , delegate_ctlc = False
            , detach_console = False
            , create_new_console = False
            , new_session = False
            , child_group = Nothing
            , child_user = Nothing
            , use_process_jobs = True
            }

makeEnv ::
       FilePath
    -> Request
    -> String
    -> String
    -> String
    -> ByteString
    -> Maybe String
    -> [(String, String)]
makeEnv baseDir req naddr scriptName pathinfo sname epath =
    addPath epath . addLen . addType . addCookie $ baseEnv
  where
    tp = baseDir </> T.unpack (T.intercalate "/" $ pathInfo req)
    baseEnv =
        [ ("GATEWAY_INTERFACE", gatewayInterface)
        , ("SCRIPT_NAME", scriptName)
        , ("REQUEST_METHOD", SB8.unpack . requestMethod $ req)
        , ("SERVER_NAME", SB8.unpack host)
        , ("SERVER_PORT", SB8.unpack port)
        , ("REMOTE_ADDR", naddr)
        , ("SERVER_PROTOCOL", show . httpVersion $ req)
        , ("SERVER_SOFTWARE", SB8.unpack sname)
        , ("PATH_INFO", pathinfo)
        , ("QUERY_STRING", query req)
        , ("PATH_TRANSLATED", tp)
        , ("GIT_HTTP_EXPORT_ALL", "TRUE")
        ]
    headers = requestHeaders req
    addLen = addLength "CONTENT_LENGTH" $ requestBodyLength req
    addType = addEnv "CONTENT_TYPE" $ lookup hContentType headers
    addCookie = addEnv "HTTP_COOKIE" $ lookup hCookie headers
    addPath Nothing ev = ev
    addPath (Just path) ev = ("PATH", path) : ev
    query = SB8.unpack . safeTail . rawQueryString
      where
        safeTail "" = ""
        safeTail bs = SB8.tail bs
    (host, port) = hostPort req

addEnv :: String -> Maybe ByteString -> [(String, String)] -> [(String, String)]
addEnv _ Nothing envs = envs
addEnv key (Just val) envs = (key, SB8.unpack val) : envs

addLength ::
       String -> RequestBodyLength -> [(String, String)] -> [(String, String)]
addLength _ ChunkedBody envs = envs
addLength key (KnownLength len) envs = (key, show len) : envs

gatewayInterface :: String
gatewayInterface = "CGI/1.1"

toCGI :: Handle -> Request -> IO ()
toCGI whdl req = runConduit $ sourceRequestBody req .| CB.sinkHandle whdl

fromCGI :: Handle -> IO Response
fromCGI rhdl = do
    (src', hs) <- cgiHeader `catch` recover
    let (st, hdr, hasBody) =
            case check hs of
                Nothing -> (internalServerError500, [], False)
                Just (s, h) -> (s, h, True)
    let src
            | hasBody = src'
            | otherwise = CL.sourceNull
    return $ responseSource st hdr src
  where
    check hs =
        lookup hContentType hs >>
        case lookup hStatus hs of
            Nothing -> Just (ok200, hs)
            Just l -> toStatus l >>= \s -> Just (s, hs')
      where
        hs' = filter (\(k, _) -> k /= hStatus) hs
    toStatus s = SB8.readInt s >>= \x -> Just (Status (fst x) s)
    emptyHeader = []
    recover (_ :: SomeException) = return (CL.sourceNull, emptyHeader)
    cgiHeader = do
        (rsrc, hs) <- CB.sourceHandle rhdl $$+ parseHeader
        src <- toResponseSource rsrc
        return (src, hs)

-- | Look-up key for Status.
hStatus :: HeaderName
hStatus = "status"

hostPort :: Request -> (ByteString, ByteString)
hostPort req =
    case requestHeaderHost req of
        Nothing -> ("Unknown", "80")
        Just hostport ->
            case SB8.break (== ':') hostport of
                (host, "") -> (host, "80")
                (host, port) -> (host, SB8.tail port)
