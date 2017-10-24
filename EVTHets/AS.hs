{-# LANGUAGE DeriveDataTypeable #-}
{-
Abstract syntax for Events
-}

module EVT.AS
        ( EVTQualId (..)
        , Sentence
        , ACTION (..)
        , GUARD (..)
        , EVENT (..)
        , MACHINE (..) 
        , EVENT_NAME
        , EVTSYMB_OR_MAP (..)
        , EVTSYMB_ITEMS (..)
        , EVTSymbol (..)
        , EVTRawSymbol (..)
        , EVTSYMB_MAP_ITEMS (..)
        , EVTBasicExt (..)
        , applyMap
        , applySymMap
        , events
                --, mapQualId
        --, getSignature
        ) where

import Data.Data
import qualified Data.Map as Map
import Data.Set as Set

import Common.Id as Id
import Common.Token
import Common.Doc
import Common.DocUtils
import Common.AS_Annotation
import Common.GlobalAnnotations

import CASL.AS_Basic_CASL
import CASL.ATC_CASL () 
import CASL.Sign

import EVT.Keywords


-- DrIFT command
type GUARD_NAME = Id
type ACTION_NAME = Id
type EVENT_NAME = Id

-- | Machines are sets of events. 
data MACHINE = MACHINE [EVENT] --Range
                 deriving (Show, Eq, Ord, Typeable, Data)

events :: MACHINE -> [EVENT]
events (MACHINE es) = es

data EVTBasicExt
  = Events [Annoted EVENT] 
  deriving (Show, Typeable, Data)



data EVENT = EVENT
                     {   name :: EVENT_NAME                            
                       , guards :: [GUARD]
                       , actions :: [ACTION]
                     }
                     deriving (Show, Eq, Ord, Typeable, Data)
data GUARD = GUARD
                     {
                        gnum :: GUARD_NAME
                      , predicate :: (FORMULA ())
                     }
                     deriving (Show, Eq, Ord, Typeable, Data)

data ACTION = ACTION
                     {
                        anum :: ACTION_NAME
                      , statement :: (FORMULA ())
                     }                                
                     deriving (Show, Eq, Ord, Typeable, Data)

data EVTQualId = EVTQualId
                {
                  eventid :: Id 
                }
                deriving (Eq, Ord, Show, Typeable, Data)


type Sentence = EVENT
                

--Symbols are fully qualified casl symbols (f:s->t), raw symbols (f) are ambiguous

data EVTSymbol = EVTguard EVTRawSymbol | 
                 EVTaction EVTRawSymbol |
                 E Id.Token -- |     -- id of an event
                   --EVTDatatype  -- datatype of the symbol
                deriving (Eq, Ord, Show, Typeable, Data)
 
data EVTSYMB_MAP_ITEMS = EvtSymb_map_items [EVTSYMB_OR_MAP] --Id.Range
                      -- pos: SYMB_KIND, commas
                      deriving (Show, Eq, Ord, Typeable, Data)

data EVTSYMB_OR_MAP = EvtSymb EVTSymbol
                 | EvtSymb_map EVTSymbol EVTSymbol --Id.Range
                   -- pos: "|->"
                   deriving (Show, Eq, Ord, Typeable, Data)

data EVTSYMB_ITEMS = EvtSymb_items [EVTSymbol] --Id.Range
                  -- pos: SYMB_KIND, commas
                  deriving (Show, Eq, Ord, Typeable, Data)

-- | application function for Symbol Maps
applySymMap :: Map.Map EVTRawSymbol EVTRawSymbol -> EVTRawSymbol -> EVTRawSymbol
applySymMap smap idt = Map.findWithDefault idt idt smap

-- | application function for Event Maps
applyMap :: Map.Map Id Id -> Id -> Id
applyMap emap idt = Map.findWithDefault idt idt emap

--RawSymbols
data EVTRawSymbol = EVTRawSymbol
                    {
                       rname :: Id.Id
                    }
                   deriving (Show, Eq, Ord, Typeable, Data)

instance Id.GetRange EVTRawSymbol where
    getRange = Id.getRange . rname

instance Pretty EVENT where
    pretty e = keyword event <+> prettyEvent e

prettyEvent :: EVENT -> Doc
prettyEvent e = prettyName e $+$ prettyGuards e  $+$ prettyActions e

prettyName :: EVENT -> Doc
prettyName e = pretty $ name e

prettyGuards :: EVENT -> Doc
prettyGuards e = 
    case guards e of
      [] -> Common.Doc.empty
      g -> prettyGuardList g

prettyGuardList :: [GUARD] -> Doc
prettyGuardList = sepBySemis . Prelude.map
  (\ (g) -> pretty g)

prettyActions :: EVENT -> Doc
prettyActions e = 
    case actions e of
      [] -> Common.Doc.empty
      a -> prettyActionList a

prettyActionList :: [ACTION] -> Doc
prettyActionList = sepBySemis . Prelude.map
  (\ (a) -> pretty a)

instance Pretty GUARD where
   pretty = prettyGuard 

prettyGuard :: GUARD -> Doc
prettyGuard g = prettyGname g <+> prettyPred g

prettyGname :: GUARD -> Doc
prettyGname g = pretty $ gnum g

prettyPred :: GUARD -> Doc
prettyPred g = pretty $ predicate g

instance Pretty ACTION where
   pretty a = prettyAname a <+> prettyStatement a

prettyAname :: ACTION -> Doc
prettyAname a = pretty $ anum a

prettyStatement :: ACTION -> Doc
prettyStatement a = pretty $ statement a

instance Pretty MACHINE where
    pretty = prettyMachine

prettyMachine :: MACHINE -> Doc
prettyMachine (MACHINE es) = printEventList es

printEventList :: [EVENT] -> Doc
printEventList = sepBySemis . Prelude.map
  (\ (ev) -> prettyName ev)


instance Pretty EVTSYMB_MAP_ITEMS where
    pretty x = pretty $ show x

instance Pretty EVTSYMB_ITEMS where
    pretty x = pretty $ show x

instance Pretty EVTSymbol where
   pretty x = pretty $ show x

{-
prettySymbolItems :: [EVTSymbol] -> Doc
prettySymbolItems = sepBySemis . Prelude.map
  (\ (sy) -> pretty sy)



map_qualId :: EVTMorphism -> EVTQualId -> Result EVTQualId
map_qualId mor qid =
    let
        (eid, rid, rn) = case qid of
            EVTQualId i1 i2 rn1 -> (i1, i2, rn1)
            return $ EVTQualId mtid mrid rn
-}

{- ^ oo-style getter function for signatures
getSignature :: RSScheme -> EVTEvents
getSignature spec = case spec of
            RSScheme tb _ _ -> tb-} 

-- Generated by DrIFT, look but don't touch!

