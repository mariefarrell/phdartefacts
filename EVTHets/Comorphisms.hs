{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}


module EVT.Comorphisms where

import Logic.Logic
import Logic.Comorphism

import EVT.StaticAnalysis (EvtBasicSpec)
import EVT.Logic
import EVT.SignCSP
import EVT.AS

import qualified Data.Set as Set

-- | The identity of the comorphism
data EVT2EVT a b = EVT2EVT a b deriving Show

instance (Show a, Show b) => Language (EVT2EVT a b) where
  language_name (EVT2EVT a b) =
      language_name (EVT a)
      ++ "2" ++ language_name (EVT b)

instance (Logic a, Logic b)
  => Comorphism (Logic a b)
    (EVT a) ()
      EvtBasicSpec EVENT EvtSymbItems EvtSymbMapItems
        EVTSign EVTMorphism EVTSymbol EVTSymbol ()
    (EVT b) ()
      EvtBasicSpec EVENT EvtSymbItems EvtSymbMapItems
        EVTSign EVTMorphism EVTSymbol EVTRawSymbol () where
    sourceLogic (EVT2EVT a _) = EVT a
    sourceSublogic _ = ()
    targetLogic (EVT2EVT _ b) = EVT b
    mapSublogic _ _ = Just ()
    map_theory _ = return
    map_morphism _ = return
    map_sentence _ = const return
    map_symbol _ _ = Set.singleton
    is_model_transportable _ = True
    has_model_expansion _ = True
    is_weakly_amalgamable _ = True
    isInclusionComorphism _ = True
