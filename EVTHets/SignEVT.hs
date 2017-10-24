{-#LANGUAGE DeriveDataTypeable,TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses#-}
module EVT.SignEVT
        ( --EVTDatatype (..)
          EventNameMap
        , EVTVarMap
        , EVTSign
	, EVTCASLMorphism (..)
        , EVTSignature (..)
        , EVTMorphism (..)
        , EVTCASLSen (..)
        , emptyEVTSignature
        , isEVTSubsig
        , isDisjoint
        , emptyEVTSign
        , eventsDisjoint
        , evtInclusion
        , uniteSig
        , uniteEVTSig
        , getSignature
        , idMorE
        , comp_mor
        , symOfE
        , getSymbolName
        , getSymbolMap
--      , addToSig
        , emptyEVTMorphism
        , mapEVTSen
        , mapEVTCASLSen
        , addToSig
        )
        where

import CASL.Sign 
import CASL.AS_Basic_CASL
import CASL.Overload
import CASL.Morphism as CASL_Morphism
import qualified CASL.MapSentence as CASL_MapSen

import EVT.AS
import EVT.Keywords

import Common.Doc
import Common.DocUtils
import Common.Id as Id
import Common.Result
import Common.Token

import Data.Data
import qualified Data.Map as Map
import Data.Set as Set

type EVTIsKey = Bool

type EventNameMap = Map.Map EVENT_NAME EVENT

type EVTVarMap = Map.Map SIMPLE_ID SORT

data EVTSignature = EVTSignature { 
                         eventNames :: Set.Set EVTRawSymbol
                      -- , varNames :: Set.Set EVTRawSymbol --but in fact I should be able to deal with this by using CASL???!
                       } deriving (Eq, Ord, Show, Typeable, Data)

type EVTSign = Sign () EVTSignature
type EVTSen = EVENT
data EVTCASLSen =  EVTCASLSen (FORMULA ()) EVTSen 
  deriving(Ord, Eq, Data, Show,Typeable)

instance GetRange EVTCASLSen where

emptyEVTSign :: EVTSign
emptyEVTSign = emptySign emptyEVTSignature

emptyEVTSignature :: EVTSignature
emptyEVTSignature = EVTSignature 
                {
                   eventNames = Set.empty
               -- ,  varNames = Set.empty
                }

data EVTMorphism = EVTMorphism
                    { domain :: EVTSign
                    , codomain :: EVTSign
                    , mapevents:: Map.Map EVTRawSymbol EVTRawSymbol
                    , mapvars:: Map.Map EVTRawSymbol EVTRawSymbol
                    }
                    deriving (Eq, Ord, Show, Typeable, Data)

{-data EVTDatatype
  = EVTBoolean | EVTNat 
    deriving (Eq, Ord, Typeable, Data)-}



getSignature:: EVTSign -> EVTSignature
getSignature a = extendedInfo a 

isEVTSubsig :: EVTSign -> EVTSign -> Bool
isEVTSubsig = isSubSig isEventSubSig

isEventSubSig :: EVTSignature -> EVTSignature -> Bool
isEventSubSig a b = (eventNames a) `Set.isSubsetOf` (eventNames b)

isDisjoint :: EVTSign -> EVTSign -> Bool
isDisjoint a b = isDisjointEVT (getSignature a) (getSignature b) 

isDisjointEVT :: EVTSignature -> EVTSignature -> Bool
isDisjointEVT a b = eventsDisjoint (eventNames a) (eventNames b)

uniteEVTSig :: EVTSignature -> EVTSignature ->EVTSignature
uniteEVTSig s1 s2 = emptyEVTSignature
                    {
                      eventNames = Set.union (eventNames s1) (eventNames s2)
               --       , varNames = Set.union (varNames s1) (varNames s2)
                    }

uniteSig :: EVTSign -> EVTSign -> Result EVTSign
uniteSig s1 s2 = do
              let newCSig = addSig (\ _ _ -> emptyEVTSignature) s1 s2
                  newEVTSign = uniteEVTSig (extendedInfo s1) (extendedInfo s2)
              return $ newCSig {extendedInfo = newEVTSign}


--instance CASL_Morphism.MorphismExtension EVTSign EVTCASLMorphism
  --  where
     -- ideMorphismExtension _ = emptyCspAddMorphism
     -- composeMorphismExtension = composeCspAddMorphism
      -- we omit inverses here
      --isInclusionMorphismExtension m =
      --  Map.null (channelMap m) && Map.null (processMap m)
      -- pretty printing for Csp morphisms
      --prettyMorphismExtension = printMap id sepByCommas pairElems
       -- . toCspSymbMap True
      --legalMorphismExtension m = do
        --checkReflCondition m
        --checkWNECondition m

evtInclusion :: EVTSign -> EVTSign ->Result EVTCASLMorphism
evtInclusion s1 s2 = 
                    let lstToIdMap xs = Map.fromAscList $ [(x, x) | x <- xs]
                        evtmor = EVTMorphism { domain = s1
                                           , codomain = s2
                                           , mapevents = (lstToIdMap . Set.toList) (eventNames $ getSignature s1)    
                       --                    , mapvars = (lstToIdMap . Set.toList) (varNames $ getSignature s1)
                                           }
                     in sigInclusion evtmor s1 s2 
eventsDisjoint :: (Ord e) => Set.Set e -> Set.Set e -> Bool
eventsDisjoint s1 s2 =
	Set.intersection s1 s2 == Set.empty


-- ^ id-morphism for EVT
idMorE :: EVTSign -> EVTMorphism
idMorE t = 
         let enames x = eventNames $ getSignature t
             evtmor = EVTMorphism{ 
                        domain = t
                        , codomain = t
                        , mapevents = Map.fromList [(x, x) | x <- Set.toList (enames t)]
           -- , mapvars = Map.fromList [(x, x) | x <- Set.toList (vnames t)]
                      }
          in evtmor


comp_mor :: EVTMorphism -> EVTMorphism -> Result EVTMorphism
comp_mor f g =
  let fSource = domain f
      gTarget = codomain g
      fMap = mapevents f
      gMap = mapevents g
  in return EVTMorphism
  { domain = fSource
  , codomain = gTarget
  , mapevents = if Map.null gMap then fMap else
      Set.fold ( \ i -> let j = applySymMap gMap (applySymMap fMap i) in
                        if i == j then id else Map.insert i j)
                                  Map.empty $ eventNames $ getSignature fSource}
--extract symbols from signature
symOfE :: EVTSign -> [Set.Set EVTSymbol]
symOfE x = [Set.fromList $ fmap (EVTguard) (Set.toList $ eventNames $ getSignature x)]

getSymbolName :: EVTRawSymbol -> Id.Id
getSymbolName = rname

getSymbolMap :: EVTMorphism -> Map.Map EVTRawSymbol EVTRawSymbol
getSymbolMap f = mapevents f

mapEVTCASLSen :: EVTCASLMorphism -> EVTCASLSen -> Result EVTCASLSen
mapEVTCASLSen ecm (EVTCASLSen f e) = return $ EVTCASLSen (CASL_MapSen.mapSen (const id) ecm f) (mapEVTSen ecm e)
 
mapEVTSen :: EVTCASLMorphism -> EVENT -> EVENT
mapEVTSen ecm (EVENT n g a) = 
  EVENT(rename n ecm) g a

rename :: EVENT_NAME -> EVTCASLMorphism -> EVENT_NAME
rename n ecm = let result = Map.lookup EVTRawSymbol{rname = n} $  mapevents $ extended_map ecm
               in case result of Nothing -> n
                                 (Just x) -> rname x

addToSig :: EVTSignature -> Id -> EVTSignature
addToSig sig tok = EVTSignature{eventNames = Set.insert EVTRawSymbol{rname = tok} $ eventNames sig}--, varNames = Set.insert EVTRawSymbol{rname = tok} $ varNames sig}

instance Pretty EVTMorphism where
    pretty = prettyEVTMorphism

prettyEVTMorphism :: EVTMorphism -> Doc
prettyEVTMorphism mor = prettyDomain mor <+> text "->" <+> prettyCodomain mor--  <+> prettyMapEvents <+> prettyMapVars

prettyDomain :: EVTMorphism -> Doc
prettyDomain mor = pretty $ domain mor

prettyCodomain :: EVTMorphism -> Doc
prettyCodomain mor = pretty $ codomain mor

instance MorphismExtension EVTSignature EVTMorphism where

instance Pretty EVTSignature where
    pretty = braces . prettyEVTSignature

prettyEVTSignature :: EVTSignature -> Doc
prettyEVTSignature sigma = prettyeventnames sigma -- <+> prettyvarnames sigma

prettyeventnames :: EVTSignature -> Doc
prettyeventnames sigma = 
    case Set.toList $ eventNames sigma of
      [] -> Common.Doc.empty
      e ->  keyword event <+> printRawList e

{-prettyvarnames :: EVTSignature -> Doc
prettyvarnames sigma = 
    case Set.toList $ varNames sigma of
      [] -> Common.Doc.empty
      v -> printRawList v-}

printRawList :: [EVTRawSymbol] -> Doc
printRawList =  sepBySemis . Prelude.map
  (\ (name) -> pretty name)

instance Pretty EVTRawSymbol where
    pretty = printSymbol

instance Pretty EVTCASLSen where
    pretty (EVTCASLSen f e) = (pretty f)<+>  (pretty e)

printSymbol :: EVTRawSymbol -> Doc
printSymbol x = pretty $ rname x

type EVTCASLMorphism = CASL_Morphism.Morphism () EVTSignature EVTMorphism

emptyEVTMorphism :: EVTMorphism
emptyEVTMorphism = EVTMorphism
    {                 domain = emptyEVTSign
                    , codomain = emptyEVTSign
                    , mapevents = Map.empty
                    , mapvars = Map.empty
    }


