--- The abstract syntax for Event-B specifications

module Syntax where
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Data.Char
import Data.List
import Data.Maybe

type Ident = String
type Predicate = String
type Expression = String

type Specification = [SpecDef]
  
data SpecDef = Machine MachineDef | Context ContextDef

instance Show SpecDef where
 show sd = 
    case sd of Machine m -> show m
               Context c -> show c
               
data MachineDef = MachineDef {
  mname :: Ident,
  mrefines :: (Maybe Ident),
  msees :: [Ident],
  mbody :: MachineBody
}deriving (Eq)

instance Show MachineDef where
  show md = let mref = (if (mrefines md) == Nothing then ""  else "\n" ++ fromJust(mrefines md) ++ " and " )
                andcons = if msees md == [] then "\n" else "\n " ++(head (msees md)) ++ concat(Prelude.map (\x -> " and " ++ (show x)) (tail (msees md)))
            in 
              "spec " ++ (mname md) ++ " = " ++ mref ++ andcons ++(if (msees md) == [] && (mrefines md) == Nothing then "" else "\n then \n") ++ (show (mbody md)) ++ "\n end \n"


data MachineBody = MachineBody {
  variables :: [(Ident, Ident)],
  invariants :: [Predicate],
  variant :: (Maybe Expression),
  initEvent :: InitEventDef,
  events :: [EventDef]
}deriving (Eq)

instance Show MachineBody where
 show mb =
  let showvars = "   ops " ++ concat (Prelude.map (\(x,y) -> (show x) ++ " : " ++ (show y) ++ "\n\t") (variables mb)) ++ "\n"
      showinvs = "   preds " ++ concat (Prelude.map (\x -> ("\n\t" ++ show x)) (invariants mb)) ++ "\n"
      showvariant =
        case (variant mb) of
          Nothing -> ""
          Just v -> "   variant " ++ show(v) ++"\n" 
      showevents = "   Events \n" ++ show (initEvent mb) ++ "\n" ++ concat (Prelude.map (\x -> show x ++ "\n" )(events mb))
   in showvars ++ showinvs ++ showvariant ++ showevents


data InitEventDef = InitEventDef {
  istatus :: Status,
  ithen :: ( [Predicate])--this had a Maybe
}deriving (Eq)
 
instance Show InitEventDef where
  show idef = "   INITIALISATION " ++ (show $ istatus idef) ++ " = \n" ++ "\t thenAct " ++ concat (Prelude.map (\x -> (show x) ++ "\n\t") (ithen idef))

data EventDef = EventDef {
  ename :: Ident,
  status :: Status,
  erefines :: [Ident],
  ebody :: EventBody
}deriving (Eq)

instance Show EventDef where
  show ed = "   " ++ "Event " ++ (show $ ename ed) ++ " " ++ (show $ status ed) ++ " = \n \t refines " ++ concat (Prelude.map show (erefines ed)) ++ "\n " ++ show (ebody ed)

data EventBody = EventBody {
  eany :: [(Ident,Ident)],
  ewhere :: [Predicate],
  ewith :: [Predicate],
  ethen :: [Predicate]
}deriving (Eq)

instance Show EventBody where
  show eb = "\t any " ++ concat(Prelude.map show (eany eb)) ++ "\n" ++ "\t where " ++ concat(Prelude.map show (ewhere eb)) ++ "\n" ++ "\t with " ++ concat(Prelude.map show (ewith eb)) ++ "\n" ++ "\t thenAct " ++ concat(Prelude.map show (ethen eb)) ++ "\n"


data Status = Ordinary | Convergent | Anticipated
  deriving (Show, Eq)

data ContextDef = ContextDef {
  cname :: Ident,
  extends :: [Ident],
  cbody :: ContextBody
}deriving(Eq)

instance Show ContextDef where
  show ct = let exlist = extends ct
                shext = if exlist == [] then "" else show (head exlist) ++ concat (Prelude.map (\x -> " and " ++ show x)(tail exlist))
             in "spec " ++ show (cname ct) ++ " = \n"++ shext ++ show (cbody ct) ++ "\n end \n"
  

data ContextBody = ContextBody {
  sets :: [Ident],
  constants :: [(Ident, Ident)],
  axioms :: [Predicate]
}deriving(Eq)

instance Show ContextBody where
  show cb = "    sorts " ++ concat (Prelude.map (\x-> "\n\t"++show x ) (sets cb)) ++ "\n    ops " ++ concat (Prelude.map (\(x,y) -> (show x) ++ " : " ++(show y)++"\n\t")(constants cb)) ++"\n    preds" ++ concat (Prelude.map (\x -> "\n\t"++show x)(axioms cb))
