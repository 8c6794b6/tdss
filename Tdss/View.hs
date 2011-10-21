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
module Tdss.View where

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as U8

import Snap.Types (Snap, Request)
import Text.Templating.Heist (Splice)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Network.URI as URI
import qualified Snap.Types as ST
import qualified Text.XmlHtml as X

import Tdss.Model (SearchResult(..))

------------------------------------------------------------------------------
-- Config values

numPageLinks :: Int
numPageLinks = 20
{-# INLINE numPageLinks #-}

perPage :: Int
perPage = 10
{-# INLINE perPage #-}

------------------------------------------------------------------------------
-- View creating functions/actions

mkResults :: [SearchResult] -> Splice Snap
mkResults rs = return [X.Element "ul" [] (map mkResult rs)]
{-# INLINE mkResults #-}

mkResult :: SearchResult -> X.Node
mkResult (SearchResult u t) =
    X.Element "li" []
      [ X.Element "div" [("id", "result")]
        [ X.Element "a" [("href", E.decodeUtf8 $ escape u)]
           [ X.TextNode $ E.decodeUtf8 u ]
        , X.Element "div" []
           [ X.TextNode $ T.take 200 $ E.decodeUtf8 t ]]]
{-# INLINE mkResult #-}    

escape :: ByteString -> ByteString
escape uri = U8.fromString (URI.escapeURIString URI.isAllowedInURI (U8.toString uri))
{-# INLINE escape #-}

mkInputQuery :: Maybe [ByteString] -> Splice Snap
mkInputQuery q = case q of
  Just (v:_) ->
    return [ X.Element "input"
             [ ("type", "text")
             , ("name", "q")
             , ("value", E.decodeUtf8 v)
             , ("size", "50") ] []]
  _ ->
    return [ X.Element "input"
             [ ("type", "text")
             , ("name", "q")
             , ("size", "50") ] []]
{-# INLINE mkInputQuery #-}    

mkSummary :: [a] -> Request -> Splice Snap
mkSummary ks req = return [element]
  where
    element = if length ks > 0 then
                 X.TextNode $ E.decodeUtf8 $ C8.concat
                   [num, " hits, page ", cur, " of ", page]
              else
                 X.TextNode $ "no hits"
    num = C8.pack . show . length $ ks
    cur = maybe (C8.pack "1") head $ ST.rqParam (C8.pack "p") req
    page = C8.pack . show $ page'
    page' = case quotRem (length ks) perPage of
              (x,0) -> x
              (x,_) -> x + 1
{-# INLINE mkSummary #-}              

mkPageLinks :: [a] -> Request -> Splice Snap
mkPageLinks ks req
  | length ks < perPage = return []
  | otherwise = do
      let q = maybe C8.empty head $ ST.rqParam "q" req
          p = maybe C8.empty head $ ST.rqParam "p" req

          currentPage = if p' == "" then 1 else read p'
             where p' = C8.unpack p

          linkURL num = E.decodeUtf8 $ U8.fromString $
                    U8.toString (ST.rqContextPath req) ++ "?" ++
                    "q=" ++ (T.unpack $ E.decodeUtf8 q) ++ "&p=" ++ show num

          mkPageLink num txt =
            X.Element "span"
              [ ("id", "page_link") ]
              [ X.Element "a"
                [ ("href", linkURL num) ]
                [ X.TextNode $ E.decodeUtf8 $ C8.pack (' ' : txt ++ " ") ]]

          mkPageLink' n = return $ mkPageLink n (' ':(show n)++" ")

          lastPage :: Int
          lastPage = case quotRem (length ks) perPage of
            (x,0) -> x
            (x,_) -> x + 1

      return [X.Element "div" [("id", "page_links")] $ catMaybes $
                [ if p == "1" || p == C8.empty
                    then Nothing
                    else Just (mkPageLink (currentPage - 1) "<< ") ]
                ++ map mkPageLink'
                       (filter (\x -> x > 0 && x <= lastPage)
                         (enumFromTo (currentPage - (numPageLinks `div` 2))
                                     (currentPage + (numPageLinks `div` 2))))
                ++
                [ if p == (C8.pack $ show lastPage)
                    then Nothing
                    else Just $ mkPageLink (currentPage + 1) " >>" ] ]
{-# INLINE mkPageLinks #-}