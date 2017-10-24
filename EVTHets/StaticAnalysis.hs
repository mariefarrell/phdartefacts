module EVT.StaticAnalysis     
    (
        evt_basic_analysis
      , evtMkStatSymbMapItem
      , evtMkStatSymbItems
      , evtStatSymbItems
      , evtSymbItemsToSymbol
      , evtSymbToSymbol
      , evtInducedFromMorphism
      , evtInducedFromToMorphism
      , caslToEVTSymbol
      , annotedEvent
    )
    where

import EVT.AS
import EVT.SignEVT

import Common.AS_Annotation
import Common.ExtSign
import Common.GlobalAnnotations
import Common.Id as Id
import Common.Result as Result
import Common.Lexer
import Common.Token
import Common.Lib.Graph
import Common.SetColimit

import CASL.AS_Basic_CASL
import CASL.Morphism
import CASL.Sign
import CASL.StaticAna
import CASL.MixfixParser

import Data.Graph.Inductive.Graph

import Control.Monad

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.List as List
import Data.Set (empty, fromList)

evt_basic_analysis :: (MACHINE, EVTSign, GlobalAnnos) -> Result (MACHINE, ExtSign EVTSign EVTSymbol, [Named EVTCASLSen])
evt_basic_analysis (spec, sign, _) = 
  let testformula = Atom True nullRange
   in
   -- do
      --(bs, ExtSign sig syms, sens) <- basicAnaAux inp
      return (spec, ExtSign
                    {
                        plainSign = makeSig spec sign 
                    ,   nonImportedSymbols = Data.Set.fromList[(EVTguard (EVTRawSymbol (stringToId "e3"))), (EVTguard (EVTRawSymbol (stringToId "e1")))]
                    },
                    (map makeNamedSen $ map annotedEvent $ Prelude.map (\x -> EVTCASLSen testformula x ) (events spec)))

annotedEvent e = Annoted {item = e, l_annos = [], r_annos = [], opt_pos = nullRange}


addEventSymbToDeclSymbs :: EVTSign -> EVTRawSymbol -> EVTSign
addEventSymbToDeclSymbs cs sy =  cs {extendedInfo  = getSignature cs `uniteEVTSig` EVTSignature{eventNames = Set.insert sy $ getEvents cs}}

getEvents :: EVTSign -> Set.Set EVTRawSymbol
getEvents e = eventNames $ getSignature e

makeSig :: MACHINE -> EVTSign -> EVTSign
makeSig (MACHINE spec) sig = List.foldl retrievebasics sig spec  ---spec = [EVENT]

retrievebasics :: EVTSign -> EVENT -> EVTSign
retrievebasics tsig e = addEventSymbToDeclSymbs tsig $ EVTRawSymbol{rname = name e} 

{-

                            
getVars :: EVTSign -> Set.Set EVTRawSymbol
getVars v = varNames $ getSignature v

retrievevars :: EVTSign -> EVENT -> EVTSign
retrievevars tsig e = addVarSymbToDeclSymbs tsig $ List.map var $ guards e

var :: GUARD -> EVTRawSymbol
var g = EVTRawSymbol{rname = predSymbName $ Predication $ predicate g} 

retrieveEvent :: EVTSignature -> EVENT -> EVTSignature
retrieveEvent tsig x = EVTSignature{eventNames = Set.insert $ name x $ eventNames tsig}-}


{-
basicAnaAux :: (MACHINE, EVTSign, GlobalAnnos)
  -> Result (MACHINE, ExtSign EVTSign Symbol, [Named EVTCASLSen])
basicAnaAux =
  basicAnalysis (const return) ana_BASIC_EVT (const return) emptyMix

ana_BASIC_EVT :: Ana EVTBasicExt EVTBasicExt () EVTSen EVTSignature
ana_BASIC_EVT mix bs = do
  let caslMix = emptyMix
        { mixRules = mixRules mix }
  case bs of
    Events es -> null [] [] -- default
  return bs-}




caslToEVTSymbol :: Symbol -> EVTSymbol
caslToEVTSymbol sy = E $ Id.idToSimpleId $ symName sy

-- | Static analysis for symbol maps
evtMkStatSymbMapItem :: [EVTSYMB_MAP_ITEMS]
                  -> Result.Result (Map.Map EVTRawSymbol EVTRawSymbol)
evtMkStatSymbMapItem xs =
    Result.Result
    {
      Result.diags = []
    , Result.maybeResult = Just $
                           foldl
                           (
                            \ smap x ->
                                case x of
                                  EvtSymb_map_items sitem ->
                                       Map.union smap $ evtStatSymbMapItem sitem
                           )
                           Map.empty
                           xs
    }

evtStatSymbMapItem :: [EVTSYMB_OR_MAP]
                 -> Map.Map EVTRawSymbol EVTRawSymbol
evtStatSymbMapItem =
    foldl
    (
     \ mmap x ->
         case x of
           EvtSymb sym ->
               Map.insert (evtSymbToSymbol sym) (evtSymbToSymbol sym) mmap
           EvtSymb_map s1 s2 ->
               Map.insert (evtSymbToSymbol s1) (evtSymbToSymbol s2) mmap
    )
    Map.empty

-- | Retrieve raw symbols
evtMkStatSymbItems :: [EVTSYMB_ITEMS] -> Result.Result [EVTRawSymbol]
evtMkStatSymbItems a = Result.Result
                    {
                      Result.diags = []
                    , Result.maybeResult = Just $ evtStatSymbItems a
                    }

evtStatSymbItems :: [EVTSYMB_ITEMS] -> [EVTRawSymbol]
evtStatSymbItems = concatMap evtSymbItemsToSymbol

evtSymbItemsToSymbol :: EVTSYMB_ITEMS -> [EVTRawSymbol]
evtSymbItemsToSymbol (EvtSymb_items syms) = map evtSymbToSymbol syms

evtSymbToSymbol :: EVTSymbol -> EVTRawSymbol
evtSymbToSymbol (E tok) = EVTRawSymbol {rname = Id.simpleIdToId tok}


makeEMap :: Map.Map EVTRawSymbol EVTRawSymbol -> EVTSign
  -> Map.Map Id.Id Id.Id
makeEMap imap sig = Set.fold ( \ x ->
  let symOf = EVTRawSymbol {rname = x}
      y = rname $ applySymMap imap symOf
  in Map.insert x y ) Map.empty $ Set.map rname $ eventNames $ getSignature sig

-- | Induce a signature morphism from a source signature and a raw symbol map
evtInducedFromMorphism :: Map.Map EVTRawSymbol EVTRawSymbol
                    -> EVTSign
                    -> Result.Result EVTCASLMorphism
evtInducedFromMorphism imap sig = let idm = idMor imap sig in return (idm emptyEVTMorphism)--let eMap = makeEMap imap sig
                                    --  mor = (idMor sig) () sig emptyEVTMorphism in
--              return mor
          --    EVTMorphism
            --              { domain = sig
              --            , mapevents = imap
                --          , codomain = sig--need to fix this
                  --        }

-- | Induce a signature morphism from a source signature and a raw symbol map
evtInducedFromToMorphism :: Map.Map EVTRawSymbol EVTRawSymbol
                    -> ExtSign EVTSign EVTSymbol
                    -> ExtSign EVTSign EVTSymbol
                    -> Result.Result EVTMorphism
evtInducedFromToMorphism imap (ExtSign sig _) (ExtSign tSig _) = 
              let
                  sigItems =  eventNames $ getSignature sig
                  newsig = getSignature sig
                  eMap :: Map.Map EVTRawSymbol EVTRawSymbol
                  eMap = Set.fold ( \ x ->
                    let symOf = x
                        y = applySymMap imap symOf
                    in Map.insert symOf y ) Map.empty sigItems
                  targetSig = newsig
                    { eventNames = sigItems }
                  isSub = Set.isSubsetOf (eventNames targetSig) (eventNames $ getSignature tSig)
              in if isSub then return EVTMorphism
                     { domain = sig
                     , mapevents = eMap
                     , codomain = tSig
                     }
                     else fail "Incompatible mapping"

signatureColimit :: Gr EVTSign (Int, EVTMorphism)
                 -> Result.Result (EVTSign, Map.Map Int EVTMorphism)
signatureColimit graph = undefined {-  do
 let graph1 = nmap eventNames $ emap (\ (x, y) -> (x, mapevents y)) graph
     (set, maps) = addIntToSymbols $ computeColimitSet graph1
     cSig = EVTSign $ getSignature {eventNames = set}
 return (cSig,
         Map.fromList $ map (\ (i, n) ->
                              (i, EVTMorphism {
                                    domain = n,
                                    codomain = cSig,
                                    mapevents = maps Map.! i
                                  })) $ labNodes graph)


{-pROPsen_analysis :: (AS_BASIC.BASIC_SPEC, Sign.Sign, AS_BASIC.FORMULA)
  -> Result.Result AS_BASIC.FORMULA
pROPsen_analysis (_, s, f) =
        let x = addFormula [] (NumForm annoF 0) s
            h = return . diagnosis . head
            g = Just . AS_Anno.sentence . formula . head
            annoF = AS_Anno.Annoted f Id.nullRange [] []
        in Result.Result (h x) (g x)-}-}
