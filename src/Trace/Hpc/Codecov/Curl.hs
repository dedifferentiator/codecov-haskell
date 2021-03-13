{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- |
-- Module:      Trace.Hpc.Codecov.Curl
-- Copyright:   (c) 2020 8c6794b6
--              (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Codecov.Curl
   ( sendJson
   , PostResult (..)
   ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS


import Network.HTTP.Client
import Network.HTTP.Client.TLS


-- | Result to the POST request to codecov.io
data PostResult =
    PostSuccess String String -- ^ Codecov job url and wait url
  | PostFailure String              -- ^ error message



-- | Send json coverage report over HTTP using POST request
sendJson :: LBS.ByteString -- ^ json coverage report
         -> String -- ^ target url
         -> Bool          -- ^ print response body if true
         -> IO (Response LBS.ByteString) -- ^ POST request result
sendJson jsonCoverage url printResponse = do
  manager <- newTlsManager

  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST"
                               , requestBody = RequestBodyLBS jsonCoverage }

  putStrLn $ "Request:\n" <> show request

  res <- httpLbs request manager
  when printResponse $ putStrLn $ "Response:\n" <> show res

  return res
