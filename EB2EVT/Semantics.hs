module Semantics where

import Syntax
import Data.List((\\), nub)
import FOPEQ as F

data FSign = FSign {
  sigsorts :: [Ident],
  sigops :: [(Ident, Ident)],
  sigpreds :: [Predicate]
}deriving(Show)

data ESign = ESign {
  sigevents :: [(Ident, Status)],
  sigvars :: [(Ident, Ident)]
  }deriving(Show)--just variable and event names for now 
e_init = "e_init"

emptySign :: Sign
emptySign = 
  Sign{
    sigS = ["BOOL"],
    sigO = [],
    sigP = [],
    sigE = [],
    sigV = []}

data Sign = Sign {
  sigS :: [Ident],
  sigO :: [(Ident, Ident)],
  sigP :: [Predicate],
  sigE :: [(Ident, Status)],
  sigV :: [(Ident, Ident)]
}deriving(Show)

mergeDifSign :: FSign -> ESign -> Sign
mergeDifSign fsig esig = 
  Sign{
    sigS = sigsorts fsig,
    sigO = sigops fsig,
    sigP = sigpreds fsig,
    sigE = sigevents esig,
    sigV = sigvars esig} 

unionFSign :: FSign -> FSign -> FSign
unionFSign f1 f2 = 
  FSign{
    sigsorts = (sigsorts f1) ++ (sigsorts f2),
    sigops = (sigops f1) ++ (sigops f2),
    sigpreds = (sigpreds f1) ++ (sigpreds f2)}

unionSign :: Sign -> Sign -> Sign
unionSign s1 s2 = 
  Sign{
    sigS = (sigS s1) ++ (sigS s2),
    sigO = (sigO s1) ++ (sigO s2),
    sigP = (sigP s1) ++ (sigP s2),
    sigE = (sigE s1) ++ (sigE s2),
    sigV = (sigV s1) ++ (sigV s2)}

-- A sentence is an (e, \phi) pair:
data Sentence = Sentence Ident Formula

instance Show Sentence where
  showsPrec d (Sentence id f) = showString "\t<" .
    showsPrec (d+1) id .
    showString "," .
    showsPrec (d+1) f .
    showString ">\n" 

-- This is what we'll store in the environment:
data Presentation = Presentation {
  sign :: Sign,
  sentences :: [Sentence]
} 

instance Show Presentation where
  showsPrec d (Presentation sig sen) = showString "Presentation\n" .
    showString "Sig=" .
    showsPrec d sig .
    showString "\nSentences are:\n" .
    showsPrec d sen .
    showString "\n"


-- filter sentences, keep those that refer to given event names
revealSentences :: Presentation -> [Ident] -> [Sentence]
revealSentences pres toKeep = filter willKeep (sentences pres)
  where willKeep (Sentence id f) = (elem id toKeep)

-- filter sentences, discard those that refer to given event names
hideSentences :: Presentation -> [Ident] -> [Sentence]
hideSentences pres toHide = filter willKeep (sentences pres)
  where willKeep (Sentence id f) = (notElem id toHide)

-- replace the event-name in a set of sentences
replaceEvent :: [Sentence] -> Ident -> [Sentence]
replaceEvent ss id = map (\ (Sentence _ f) -> (Sentence id f)) ss


-- The environment (should really be: Ident -> Presentation)
type Env = [(Ident, Presentation)] 

-- get the definition of an ident from an environment
getEnv :: Env -> Ident -> Presentation
getEnv ((id1,p):eTail) id = if (id==id1) then p else (getEnv eTail id)


----- Semantic functions start here -----

bbSpec :: Specification -> Env
bbSpec ss = foldl bbs [] ss
  where bbs env (Machine md) = bbMacDef md env
        bbs env (Context cd) = bbCtxDef cd env

iota :: [(Ident, Ident)] -> [(Ident, Ident)]
iota vl = Prelude.map (\(x,y) -> (x++"'", y))vl

bbMacDef :: MachineDef -> Env -> Env
bbMacDef md env = 
  let
      thisBody = (mbody md)
      seesCon = msees md
      seesConSigs = bbSeesS seesCon env
      abstractEvents = retrieveEventNames (mrefines md) env
      abstractVars = retrieveVarNames(mrefines md) env
      refinedEvents = getRefinedEvents thisBody
      unrefinedEvents =  abstractEvents \\ refinedEvents
      myEvents = (getDefinedEvents thisBody) ++ unrefinedEvents
      myVars = (getDefinedVars thisBody) ++ abstractVars
      xx' = myVars ++ (iota myVars)
      msig = Sign{sigS = [], sigO = [], sigP =[],sigE = myEvents, sigV = myVars}
      msen = Presentation {
        sign = unionSign seesConSigs msig,
        sentences = (concat [
            -- (1) Pull in sentences from unrefined abstract events:
            (bbRefines (map fst unrefinedEvents) (mrefines md) env),
            (bbEventRefines (events thisBody) (mrefines md) env),
            -- (2) Pull in sentences from seen contexts:
            (bbSees (msees md) env),
            -- (3) Generate invariants and variants:
            (assertInvariants xx' myEvents (invariants thisBody)),
            (assertVariant (variant thisBody) (events thisBody)),
            -- (4) Generate sentences from events actually declared here:
            (bbInitEventDef (xx')(initEvent thisBody) env),
            (bbEventDefines (xx') (events thisBody))
          ])
      }
  in
    ((mname md), msen) : env


-- Assert the invariants for events declared in this machine:
assertInvariants :: [(Ident,Ident)]->[(Ident, Status)] ->  [Predicate] ->  [Sentence]
assertInvariants xx' evtNames invs =
  let makeAssert evt inv = (Sentence evt (F.forall xx' inv)) 
      assertOne evt = map (makeAssert evt) invs
      enames = [x | (x,y) <- evtNames]
  in concat (map assertOne enames)

-- Assert that the variant is not increased for relevant events in this machine:
assertVariant :: (Maybe Expression) -> [EventDef] -> [Sentence]
assertVariant Nothing evtList = []
assertVariant (Just var) evtList =
  let assertv name Ordinary soFar = soFar
      assertv name Convergent soFar  = (Sentence name (F.lt var var)) : soFar
      assertv name Anticipated soFar = (Sentence name (F.le var var)) : soFar
      assertOne soFar evt = assertv (ename evt) (status evt) soFar
  in foldl assertOne [] evtList

-- Generate sentences for the init event
bbInitEventDef :: [(Ident, Ident)]->InitEventDef -> Env -> [Sentence]
bbInitEventDef xx' init env =[(Sentence e_init (F.forall xx' (concat $ ithen init)))]
 {- let post Nothing = []
      post (Just p) = 
  in post (ithen init) --hack-} 

-- Pull in all the sentences for any unrefined abstract events
bbRefines :: [Ident] -> (Maybe Ident) -> Env -> [Sentence]
bbRefines unrefined Nothing env = []
bbRefines unrefined (Just id) env =
  let absmac = (getEnv env id)
  in revealSentences absmac unrefined

-- Get the event names from a machine in the environment
retrieveEventNames :: (Maybe Ident) -> Env -> [(Ident, Status)]
retrieveEventNames Nothing env = []
retrieveEventNames (Just id) env = sigE(sign (getEnv env id))

-- Get the variable names from a machine in the environment
retrieveVarNames :: (Maybe Ident) -> Env -> [(Ident, Ident)]
retrieveVarNames Nothing env = []
retrieveVarNames (Just id) env = sigV(sign (getEnv env id))
-- Collect the names of the defined events from all event defs
getDefinedEvents :: MachineBody -> [(Ident, Status)]
getDefinedEvents mb = (Prelude.map (\x -> (ename x, status x))(events mb)) ++ [("e_init", Ordinary)]

getDefinedVars :: MachineBody -> [(Ident, Ident)]
getDefinedVars mb = (variables mb)

-- Collect the names of the refined events from all event defs
getRefinedEvents :: MachineBody -> [(Ident, Status)]
getRefinedEvents mb = 
  let refinedevs = nub (concat (map erefines (events mb)))
   in Prelude.map (\x -> (x, Ordinary)) refinedevs


-- Generate the sentences for event definitions (G && W && BA)
bbEventDefines ::[(Ident, Ident)]-> [EventDef] -> [Sentence]
bbEventDefines xx' evtList =
  let pp eBody = (eany eBody)
      gg eBody = F.and (ewhere eBody) 
      ww eBody = F.and (ewith eBody)
      ba eBody = F.and (ethen eBody)
      bbEvent eBody = (F.forall (xx' ++(pp eBody)) (F.and [gg eBody, ww eBody, ba eBody]))
      bbEventDef evt = (Sentence (ename evt) (bbEvent (ebody evt)))
  in (map bbEventDef evtList)

-- Pull in sentences from refined events, and rename them to concrete event
bbEventRefines :: [EventDef] -> (Maybe Ident) -> Env -> [Sentence]
bbEventRefines _ Nothing _ = []
bbEventRefines evtList (Just absmac) env =
  let refineMe concEvt absEvts =
        let absPres = (getEnv env absmac) 
            absSentences = (revealSentences absPres absEvts) :: [Sentence]
        in (replaceEvent absSentences concEvt)   :: [Sentence]
      bbEventRef evt = (refineMe (ename evt) (erefines evt))  :: [Sentence]
  in concat (map bbEventRef evtList)

-- Pull in sentences from seen contexts 
bbSees :: [Ident] -> Env -> [Sentence]
bbSees ids env =
  concat [sentences (snd y) | x<-ids, y<-env, fst y == x]

bbSeesS :: [Ident] -> Env -> Sign
bbSeesS ids env = 
  let slist = [sign (snd y) | x<-ids, y<-env, fst y ==x]
   in case slist of [] -> emptySign
                    x:xs -> unionSignList (x:xs)

unionSignList :: [Sign] -> Sign
unionSignList slist = 
  case slist of
    []-> emptySign
    x: xs -> (unionSign x (unionSignList xs))

-- Process a context definition (not done yet)
bbCtxDef :: ContextDef -> Env -> Env
bbCtxDef cd env = ((cname cd), csen) : env
  where csen =
          Presentation
          { sign = rhoSign $ FSign{sigsorts = sets $ cbody cd, sigops = constants $ cbody cd, sigpreds = axioms $ cbody cd},
            sentences = Prelude.map rhoSen (axioms $ cbody cd )}

rhoSign :: FSign -> Sign
rhoSign f = 
  Sign{
    sigS = sigsorts f,
    sigO = sigops f,
    sigP = sigpreds f,
    sigE = [],
    sigV = []}

rhoSen :: Predicate -> Sentence
rhoSen p = Sentence e_init p
