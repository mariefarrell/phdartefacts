module EVT.SymbolParser
  ( evtSymbItems
  , evtSymbMapItems
  , evtSymb
  , evtSymbMap
  , evtSymbMaps
  ) where

import CASL.SymbolParser
import EVT.AS
import EVT.Keywords
import EVT.SignEVT

import Common.AnnoState
import Common.DocUtils
import Common.Id
import Common.Keywords
import Common.Lexer
import Common.Token
import Common.Parsec

import Text.ParserCombinators.Parsec

-- | Any word to token
evtId :: GenParser Char st Token
evtId = pToken $ reserved evtKeywords scanAnyWords

-- | parsing an evt symbol
evtSymb :: GenParser Char st EVTSymbol
evtSymb = fmap E evtId

-- | parsing one symbol or a mapping of one to a second symbol
evtSymbMap :: GenParser Char st EVTSYMB_OR_MAP
evtSymbMap = do
  s <- evtSymb
  do f <- pToken $ toKey mapsTo
     t <- evtSymb
     return (EvtSymb_map s t {-$ tokPos f-})
    <|> return (EvtSymb s)

-- | Parse a list of comma separated symbols.
evtSymbItems :: GenParser Char st EVTSYMB_ITEMS
evtSymbItems = do
  (is, ps) <- evtSymbs
  return (EvtSymb_items is)

-- | parse a comma separated list of symbols
evtSymbs :: GenParser Char st ([EVTSymbol], [Token])
evtSymbs = do
       s <- evtSymb
       do c <- commaT `followedWith` evtSymb
          (is, ps) <- evtSymbs
          return (s : is, c : ps)
         <|> return ([s], [])

-- | parse a list of symbol mappings
evtSymbMapItems :: GenParser Char st EVTSYMB_MAP_ITEMS
evtSymbMapItems = do
  (is, ps) <- evtSymbMaps
  return (EvtSymb_map_items is)

-- | parse a comma separated list of symbol mappings
evtSymbMaps :: GenParser Char st ([EVTSYMB_OR_MAP], [Token])
evtSymbMaps = do
  s <- evtSymbMap
  do c <- commaT `followedWith` evtSymb
     (is, ps) <- evtSymbMaps
     return (s : is, c : ps)
    <|> return ([s], []) 
