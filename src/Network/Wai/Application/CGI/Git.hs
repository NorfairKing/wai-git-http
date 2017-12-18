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

import Control.Exception (bracket, catch)
import Control.Exception (SomeException(..))
import Control.Monad

import System.Environment (lookupEnv)
import System.IO
import System.Process

import Network.HTTP.Types
import Network.SockAddr (showSockAddr)
import Network.Wai
import Network.Wai.Conduit

import Network.Wai.Application.Classic.Conduit
       (parseHeader, toResponseSource)
import Network.Wai.Application.Classic.Field (textPlainHeader)
import Network.Wai.Application.Classic.Header (hStatus, hostPort)

cgiGitBackend :: Application
cgiGitBackend req respond = do
    case parseMethod $ requestMethod req of
        Right GET -> cgiGitBackendApp False req respond
        Right POST -> cgiGitBackendApp True req respond
        _ ->
            respond $
            responseLBS
                methodNotAllowed405
                textPlainHeader
                "Method Not Allowed\r\n"

cgiGitBackendApp :: Bool -> Application
cgiGitBackendApp body req respond = bracket setup teardown (respond <=< cgi)
  where
    setup = execGitBackendProcess req
    teardown (rhdl, whdl, pid) = do
        terminateProcess pid -- SIGTERM
        hClose rhdl
        hClose whdl
    cgi (rhdl, whdl, _) = do
        when body $ toCGI whdl req
        hClose whdl -- telling EOF
        fromCGI rhdl

execGitBackendProcess :: Request -> IO (Handle, Handle, ProcessHandle)
execGitBackendProcess req = do
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
        }

makeEnv ::
       Request
    -> String
    -> String
    -> String
    -> ByteString
    -> Maybe String
    -> [(String, String)]
makeEnv req naddr scriptName pathinfo sname epath =
    addPath epath . addLen . addType . addCookie $ baseEnv
  where
    tp =
        case pathInfo req of
            [] -> error "Should never happen." -- FIXME
            (_:rest) ->
                "/tmp/shared-wolf/accounts/379cf27b-84c1-44bd-a73f-1f4315f5be9c/data/.git/" ++
                T.unpack (T.intercalate "/" rest)
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
toCGI whdl req = sourceRequestBody req $$ CB.sinkHandle whdl

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
