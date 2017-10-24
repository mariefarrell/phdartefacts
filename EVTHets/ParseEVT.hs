module EVT.ParseEVT
        (
        evtBasicSpec
        , parseEVTGuards
        , parseEVTActions
        , parseGuard
        , parseAction
        )
        where

import Common.AS_Annotation
import Common.AnnoState
import Common.Id
import Text.ParserCombinators.Parsec
import Common.GlobalAnnotations (PrefixMap)
import Common.Token 
import Common.Parsec
import Common.Lexer
import Common.Keywords

import CASL.AS_Basic_CASL
import qualified CASL.Formula as CASL

import EVT.AS 
import EVT.Keywords
import EVT.SymbolParser

evtBasicSpec :: PrefixMap -> AParser st MACHINE
evtBasicSpec _ = do spaces
 --                 pos1 <- getPos
                    es <- many parseEVTEvents                    
   --               pos2 <- getPos
                    return (MACHINE es)
parseEVTEvents ::AParser st EVENT
parseEVTEvents =
        do
           try $ asKey event
           es <- parseEvent   
           return es

parseEvent ::AParser st EVENT
parseEvent =
        do
           name <- theName
           char '='  
           try $ asKey when
           gs <- many parseEVTGuards
           try $ asKey thenact
           as <- many parseEVTActions
           return (EVENT name gs as)

parseEVTGuards ::AParser st GUARD
parseEVTGuards=
        do
           gs <- parseGuard   
           return gs

theName :: AParser st Id
theName = sortId evtKeywords

parseGuard :: AParser st GUARD
parseGuard= do 
              spaces
              gid<-theName
              spaces
              pr<-CASL.formula evtKeywords
              return GUARD 
               {
                 gnum = gid
                 , predicate = pr
               }

parseEVTActions :: AParser st ACTION
parseEVTActions=
        do
           as <- parseAction   
           return as

parseAction :: AParser st ACTION
parseAction =  do 
              spaces
              aid<- theName 
              spaces
              st<- CASL.formula evtKeywords
              return ACTION 
               {
                anum = aid
                , statement = st
               }        

iId :: GenParser Char st Token
iId = pToken $ reserved evtKeywords scanAnyWords

