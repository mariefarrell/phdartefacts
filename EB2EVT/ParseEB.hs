{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where
import Text.XML.HXT.Core
import Syntax
import Semantics
import Data.List as List
import System.Directory as Directory
import Control.Monad (forM)
import Data.Strings


mkSpecDefMac :: MachineDef -> SpecDef
mkSpecDefMac m = Machine m

mkSpecDefCon :: ContextDef -> SpecDef
mkSpecDefCon m = Context m


parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
 
atTag tag = deep (isElem >>> hasName tag)

maname = "testmachine"

mref = atTag "org.eventb.core.refinesMachine" >>>
  proc l -> do
    mr <- getAttrValue "org.eventb.core.target" -< l  
    returnA -< (mr)

masees = atTag "org.eventb.core.seesContext" >>>
  proc l -> do
    ms <- getAttrValue "org.eventb.core.target" -< l
    returnA -< (ms)

getVariables = atTag "org.eventb.core.variable" >>>
  proc l -> do
    vid <- getAttrValue "org.eventb.core.identifier" -< l
  --  vtype <- getAttrValue "org.eventb.core.type" -< l
    returnA -< (vid)
 
getInvariants = atTag "org.eventb.core.invariant" >>>
  proc l -> do
    ipred <- getAttrValue "org.eventb.core.predicate" -< l
    ilabel <- getAttrValue "org.eventb.core.label" -< l
    returnA -< (ipred, ilabel)

getvariant = atTag "org.eventb.core.variant" >>>
  proc l -> do
    vexp <- getAttrValue "org.eventb.core.expression" -< l
    returnA -< (vexp)

strContains:: String -> Char -> Bool
strContains s c = or $ Prelude.map(\str -> str == c)s

varTypes inv vid = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_") || (strEndsWith y "_type")||(strContains x '\8712')) inv
      match1 = [(vname, p) | vname <- vid, (p,x) <- types, x == "typeof_"++vname]
      match2 =  [(vname, p) | vname <- vid, (p,x) <- types, x == vname ++ "_type"]
      match3 = [(vname, strDrop ((strLen vname)+3) x) |  vname <- vid, (x,y) <- inv, strStartsWith x (vname ++ " \8712 ")]
      match = match1++match2 
  in (Prelude.map (\(x,y) -> (x, strDrop ((strLen x) + 3) y)) match) ++match3

removeTypeInvariants il = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_")||(strEndsWith y "_type")||(strContains x '\8712')) il
      notypes = il\\types
   in Prelude.map fst notypes

getMachine name = atTag"org.eventb.core.machineFile">>>
  proc l -> do
    macrefines <- listA mref -< l
    macsees <- listA masees -< l
    mvariables <- listA getVariables -< l
    minvariants <- listA getInvariants -< l
    mvariant <- listA getvariant -< l
    mevents <- listA getEvents -<l
    returnA -< Machine (MachineDef{
                mname = name,
                mrefines = if macrefines == [] then Nothing else Just $ head macrefines, -- should be Just
                msees = macsees,
                mbody = MachineBody {
                    variables = varTypes minvariants mvariables,
                    invariants = Prelude.map (primeinvs mvariables) (removeTypeInvariants minvariants),
                    variant = if mvariant == [] then Nothing else Just(head$ mvariant),
                    initEvent = getInitEvent mevents,
                    events = removeInitEvent mevents} })
    
primeinvs :: [String] -> String -> String
primeinvs vl pred = pred ++ "&"++ primevars vl pred

primevars :: [String]-> String -> String
primevars vl s =
  case vl of
    [] -> s
    x:xs ->  primevars xs (strReplace x (ps x) s)

ps :: String -> String
ps s  = s ++ "'"
  
getEvents = atTag "org.eventb.core.event" >>>
  proc l -> do
    evname <- getAttrValue "org.eventb.core.label" -< l
    estat <- getAttrValue "org.eventb.core.convergence" -< l
    erefine <-  listA getRefinesEvents -< l
    any <- listA getAnys -< l
    eguard <- listA getGuards-<l
    with <- listA getWitnesses-<l
    eact <-  listA getActs-<l
    returnA-< (EventDef {
                           ename = evname,
                           status = makeStatus estat,
                           erefines = erefine,
                           ebody = EventBody{eany = getAnysTypes eguard any, ewhere = removeTypeAnyGuards eguard, ewith = with,ethen =Prelude.map prime eact}
                          })
  
getRefinesEvents =  atTag "org.eventb.core.refinesEvent" >>>
  proc l -> do
    refine <- getAttrValue "org.eventb.core.target" -< l
    returnA -< refine
    
getWitnesses =  atTag "org.eventb.core.witness" >>>
  proc l -> do
    witness <- getAttrValue "org.eventb.core.predicate" -< l
    returnA -< witness
  
  
getAnys =  atTag "org.eventb.core.parameter" >>>
  proc l -> do
     param <- getAttrValue "org.eventb.core.identifier"-< l
     returnA -< param

getAnysTypes grds aid = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_")|| (strEndsWith y "_type")) grds
      match = [(vname, p) | vname <- aid, (p,x) <- types, (x == "typeof_"++vname) || (x == vname ++ "_type")]
   in Prelude.map (\(x,y) -> (x, strDrop ((strLen x) + 3) y)) match

removeTypeAnyGuards grds = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_") ||(strEndsWith y "_type")) grds
      notypes = grds\\types
   in Prelude.map fst notypes

getGuards = atTag "org.eventb.core.guard" >>>
 proc l -> do
   guardval<- getAttrValue "org.eventb.core.predicate"  -< l
   gid <- getAttrValue "org.eventb.core.label" -<l
   returnA-< (guardval, gid)

getActs = atTag "org.eventb.core.action" >>>
 proc l -> do
   actval<- getAttrValue "org.eventb.core.assignment"  -< l
   returnA-< actval

prime:: String -> String -- this is not the right way to do these but it is ok for simple assignment actions
prime actval = sReplace " \8788" "' =" actval
  
makeStatus :: String -> Status
makeStatus s = case s of 
  "0" -> Ordinary
  "1" -> Anticipated
  "2" -> Convergent --not sure that these are correct, check them.

getContext name = atTag "org.eventb.core.contextFile">>>
  proc l -> do
    cextends <- listA getExtendsContexts -< l
    carriers <- listA getCarriers -< l
    cons <- listA getConstants -< l
    ax <- listA getAxioms -< l
    returnA-< Context(ContextDef{
                  cname = name,
                  extends = cextends,
                  cbody = ContextBody{
                      sets = carriers,
                      constants = getConsTypes ax cons,
                      axioms = removeTypeAxioms ax}}
                      )

getExtendsContexts = atTag "org.eventb.core.extendsContext" >>>
  proc l -> do
    con <- getAttrValue "org.eventb.core.target" -< l
    returnA-< con
  
getCarriers = atTag "org.eventb.core.carrierSet" >>>
  proc l -> do
    set <- getAttrValue "org.eventb.core.identifier"  -< l
    returnA -< set

getConstants = atTag "org.eventb.core.constant" >>>
  proc l -> do
    consname <- getAttrValue "org.eventb.core.identifier" -< l
    returnA -< consname

getAxioms = atTag "org.eventb.core.axiom" >>>
  proc l -> do
    axpred <- getAttrValue "org.eventb.core.predicate" -< l
    axlabel <- getAttrValue "org.eventb.core.label" -< l
    returnA -< (axpred, axlabel)

getConsTypes cax cid = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_")|| (strEndsWith y "_type")) cax
      match = [(vname, p) | vname <- cid, (p,x) <- types, (x == "typeof_"++vname) || (x == vname ++ "_type")]
   in Prelude.map (\(x,y) -> (x, strDrop ((strLen x) + 3) y)) match

removeTypeAxioms axl = 
  let types = filter (\(x,y) -> (strStartsWith y "typeof_") ||(strEndsWith y "_type")) axl
      notypes = axl\\types
   in Prelude.map fst notypes

statToString :: Status -> String
statToString s = 
  case s of 
    Ordinary -> "ordinary"
    Anticipated -> "anticipated"
    Convergent -> "convergent"

parseFile ::FilePath -> IO[SpecDef]
parseFile t =
  case
    ebtype t of "machine" ->  do
                  m <- runX (parseXML t >>> getMachine (takeWhile (/='.') t))
                  return m
                "context" -> do
                  c <- runX (parseXML t >>> getContext (takeWhile (/='.') t))
                  return c

parseDirectory :: FilePath -> IO ()
parseDirectory dir = do
  files<- getDirectoryContents dir
  let ebfiles =filter ebfile files
  contents <- mapM parseFile ebfiles
  writeFile (dir++"testparsedir.evt") (show $ bbSpec $ orderSpecs $concat contents)
  writeFile (dir++"testparsedirToMEB.evt") (show $ orderSpecs $concat contents) 

ebfile :: String -> Bool
ebfile s =
  let ext = List.dropWhile (/= '.') s
   in
    case ext of
      ".bum" -> True
      ".buc"-> True
      _ -> False

    
mkSpecDefM :: [MachineDef] -> [SpecDef]
mkSpecDefM ml = Prelude.map mkSpecDefMac ml

mkSpecDefC :: [ContextDef] -> [SpecDef]
mkSpecDefC cl = Prelude.map mkSpecDefCon cl

refchain :: [MachineDef] ->[MachineDef] -> [MachineDef]
refchain l ml =
  if ml == [] then [] else
    case l of
      [] ->
        let abs = [x | x<-ml, mrefines x == Nothing]
         in abs ++ refchain abs (ml\\abs)
      x:xs -> 
        let ref = [y | y <- ml, mrefines y == Just (mname x)]
          in ref ++ (refchain ref (ml\\ref))

extchain :: [ContextDef] ->[ContextDef] -> [ContextDef]
extchain l cl =
  if cl == [] then [] else
    case l of
      [] ->
        let abs = [x | x<-cl, extends x == []]
         in abs ++ extchain abs (cl\\abs)
      x:xs -> 
        let ext = [y | y <- cl, elem (cname x)(extends y) ]
          in ext ++ (extchain ext (cl\\ext))

orderSpecs :: [SpecDef]->[SpecDef]
orderSpecs sl =
  let cons =  [c | c<-sl, isContext c]
      macs = [m |m<-sl, not(isContext m)]
      rawcons = Prelude.map (\(Context c) -> c) cons
      ordrawcons = extchain [] rawcons
      ordcons = Prelude.map (\c -> (Context c)) ordrawcons
      rawmacs = Prelude.map (\(Machine m) -> m) macs
      ordrawmacs = refchain [] rawmacs
      ordmacs = Prelude.map (\m -> (Machine m)) ordrawmacs
   in ordcons ++ ordmacs

isContext :: SpecDef -> Bool
isContext s =
  case s of
    Context c -> True
    Machine m -> False
    
ebtype :: String  -> String
ebtype t =
  let ext = List.dropWhile (/= '.') t
   in
    case ext of
      ".bum" -> "machine"
      ".buc"-> "context"
      _ -> error "Invalid Input"

  
getInitEvent:: [EventDef] -> InitEventDef
getInitEvent elist =
  let x = List.find (\x -> (ename x) == "INITIALISATION") elist
   in
    case x of Nothing -> iev
              Just i -> mkInitEventDef i
              
removeInitEvent :: [EventDef] -> [EventDef]
removeInitEvent elist =
  List.filter (\x -> (ename x) /= "INITIALISATION") elist
              
mkInitEventDef :: EventDef -> InitEventDef
mkInitEventDef e =
  InitEventDef{
    istatus = status e,
    ithen = (ethen $ ebody e)}

iev =  InitEventDef {
  istatus =Ordinary,
  ithen = []
}
main :: IO()
main = print "done"

