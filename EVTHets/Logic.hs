{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{- 

Instance of class Logic for EVTs
-}

module EVT.Logic where

import Common.DocUtils
import Common.Id
import Common.ExtSign
import Common.AS_Annotation
import Common.ExtSign
import Common.GlobalAnnotations
import Common.Id
import Common.Result
import Data.Set (empty, fromList)
import Data.Map as Map
import Data.Monoid
import Data.Data
import Logic.Logic

import EVT.AS
import EVT.SignEVT
import EVT.ParseEVT
import EVT.ATC_EVT ()
import EVT.StaticAnalysis
import EVT.SymbolParser

import CASL.Logic_CASL
import CASL.Sign
import CASL.Morphism

import Data.Set as Set

data EVT = EVT deriving (Show)

instance Language EVT where
    description _ =
        "Logic for the Institution EVT" 


{-instance Category
        EVTSign                    -- sign
        EVTCASLMorphism             -- mor
        where
                dom = msource 
                cod = mtarget
                ide sig = idMor (ideMorphismExtension $ extendedInfo sig) sig
                composeMorphisms = composeM composeMorphismExtension --comp_mor
-} 
-- | Instance of Sentences for EVT
instance Sentences EVT EVTCASLSen EVTSign EVTCASLMorphism EVTSymbol where
   simplify_sen EVT _ form = form
   print_named _ = printAnnoted pretty . fromLabelledSen
   sym_of EVT = symOfE
   map_sen EVT =  mapEVTCASLSen --- need this for with


instance SignExtension EVTSignature where
    isSubSignExtension _ _ = True

instance Monoid EVENT where
     mempty = EVENT (stringToId "") [] [] 
     mappend (EVENT n1 g1 a1) (EVENT n2 g2 a2) = EVENT (mappend n1 n2) (mappend g1 g2) (mappend a1 a2) 

instance Monoid MACHINE where
     mempty = MACHINE []
     mappend (MACHINE m1) (MACHINE m2) = MACHINE (mappend m1 m2) 

instance Monoid EVTSign where
     mempty = emptySign emptyEVTSignature
     mappend s1 s2 = undefined --EVTSign (mappend c1 c2) (mappend s1 s2)

instance Monoid EVTSignature where
    mempty = emptyEVTSignature
    mappend (EVTSignature e1) (EVTSignature e2) = EVTSignature (mappend e1 e2) 

instance Monoid Id

-- | Syntax of EVT
instance Syntax EVT MACHINE EVTSymbol EVTSYMB_ITEMS EVTSYMB_MAP_ITEMS where
     parse_basic_spec _ = Just $ evtBasicSpec
     parse_symb_items _ = Just evtSymbItems
     parse_symb_map_items _ = Just evtSymbMapItems

instance Logic EVT
    ()-- Sublogics (missing)
    MACHINE -- basic_spec    
    EVTCASLSen   -- sentence
    EVTSYMB_ITEMS -- symb_items
    EVTSYMB_MAP_ITEMS --symb map items
    EVTSign  -- sign
    EVTCASLMorphism --morphism
    EVTSymbol  --symbol
    EVTRawSymbol --raw_symbol
    ()-- proof_tree (missing)
    where
      stability (EVT)= Experimental
      data_logic (EVT) = Just (Logic CASL)
      empty_proof_tree _ = ()
     -- provers (GenCspCASL _) = cspProvers (undefined :: a)

-- | Static Analysis for EVT
instance StaticAnalysis EVT
    MACHINE                    -- basic_spec
    EVTCASLSen--   Sentence
    EVTSYMB_ITEMS                            -- symb_items
    EVTSYMB_MAP_ITEMS                            -- symb_map_items
    EVTSign  -- sign                
    EVTCASLMorphism                    -- morphism
    EVTSymbol                      -- symbol
    EVTRawSymbol                   -- raw_symbol
    where
      basic_analysis EVT = Just evt_basic_analysis
      empty_signature EVT = emptyEVTSign 
      is_subsig EVT = isEVTSubsig
      subsig_inclusion EVT = evtInclusion
      signature_union EVT = uniteSig
      stat_symb_items EVT _ = evtMkStatSymbItems
      stat_symb_map_items EVT  _ _ = evtMkStatSymbMapItem
      induced_from_morphism EVT = evtInducedFromMorphism 
    --  induced_from_to_morphism EVT = evtInducedFromToMorphism
instance GetRange EVENT where
instance GetRange MACHINE where
instance GetRange EVTSymbol where


