{- |
Module      :  $Header$
Description :  CspCASL keywords to be used for parsing and printing
Copyright   :  (c) Andy Gimblett and Swansea University 2006
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  a.m.gimblett@swan.ac.uk
Stability   :  provisional
Portability :  portable

String constants for CspCASL keywords to be used for parsing and
printing.

-}

module EVT.Keywords where 
import Common.Token

-- | Keywords identifying beginning of eventdeclaration part.
event :: String
event = "event"

inv :: String
inv = "inv"

when :: String
when = "when"

grd :: String
grd = "grd"

thenact :: String
thenact = "thenAct"

act :: String
act = "act"

end :: String
end = "end"

assign:: String
assign = ":="

checkeq :: String
checkeq = "="

member :: String
member = ":"

thenaturalnumbers:: String
thenaturalnumbers = "thenats"


-- | starting EVT keywords
startevtKeywords :: [String]
startevtKeywords =
  [ event
  , when
  , thenact
  , grd
  , act]

-- | Reserved keywords specific to EVT.
evtKeywords :: [String]
evtKeywords = criticalKeywords ++ startevtKeywords ++
  [ assign, checkeq]
