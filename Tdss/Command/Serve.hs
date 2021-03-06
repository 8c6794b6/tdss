{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Tdss.Command.Serve where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newMVar, readMVar)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import qualified Data.ByteString.Char8 as C8

import Blaze.ByteString.Builder (toByteString)
import Control.Monad.Trans (liftIO)
import Snap.Http.Server
import Snap.Types
import Text.Templating.Heist (TemplateState)
import qualified Snap.Util.FileServe as FS
import qualified Text.Templating.Heist as HE

import qualified Tdss.Control as C

-- | Serve the site.
run :: Int      -- ^ Port number
    -> FilePath -- ^ Database path
    -> FilePath -- ^ Template path
    -> IO ()
run pNum dPath tPath = do
  ets <- HE.loadTemplates tPath (HE.emptyTemplateState "")
  let ts = either error id ets
      conf = setPort pNum defaultConfig
  maybeMakeLogDir conf
  tsMVar <- newMVar ts
  httpServe conf (site dPath tsMVar)
{-# INLINE run #-}

maybeMakeLogDir :: Config m a -> IO ()
maybeMakeLogDir conf = case getAccessLog conf of
  Just (Just logf) -> createDirectoryIfMissing True $ fst (splitFileName logf)
  _                -> return ()
{-# INLINE maybeMakeLogDir #-}

-- | Main url mapping.
site :: FilePath -> MVar (TemplateState Snap) -> Snap ()
site db tsMVar =
  FS.serveDirectory "./"
  <|> route [("", C.queryPhrase db tsMVar)]
  <|> templateServe tsMVar
{-# INLINE site #-}

-- | Serves templates with state in MVar.
templateServe :: MVar (TemplateState Snap) -> Snap ()
templateServe tsMVar = do
  ts <- liftIO $ readMVar tsMVar
  urlPath <- return . maybe "search" id . urlDecode . C8.pack =<< FS.getSafePath
  maybe pass (writeBS . toByteString . fst) =<< HE.renderTemplate ts urlPath
  modifyResponse $ setContentType "text/html"
{-# INLINE templateServe #-}