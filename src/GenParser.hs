{-# LANGUAGE RecordWildCards #-}

module GenParser where
-- write an haskell source for the parser AST

import Compiler
import W3cData
import Utils
import Debug

import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Data.Map as Map
import Control.Monad
import Control.Arrow

-- genData::EBNF->Either String String
-- genData e@EBNF{..}=genData' (genMap e) ("genData"§_entry)

genData'::CompExprMap->String->Either String String
genData' m n =Right (cm,
      do
        (MetaResult r')<-cc$c
        return $MetaResult r'
  ) where
    c=genRuleData nm n=<<rule::Either String String
    Right (cm,ce)=c
    nm=Map.insert n (Left ce) m::CompExprMap
    rule=Map.lookup n m ~> ("Rule <"++n++"> not found"::String)

genRuleData::CompExprMap->String->CompExpr->Either String String
genRuleData m _ (Left c) = Right (m,c) -- already compiled (avoid checking infinite parsers)
genRuleData m n (Right e) = genExprData m n e -- ask for compilation

genExprData::CompExprMap->String->Expr->Either String String
genExprData m n (Alt (o:oo))= (\x->foldM f x oo)=<<r
  where
    r=genSeqData m n o
    f::String->Seq->Either String String
    f (ma,ca) b=second (ca<||>)<$>genSeqData ma n b
genExprData m n (Expr o)=genSeqData m n o

genSeqData::CompExprMap->String->Seq->Either String String
genSeqData m n (Mods [])=error "empty mods list"
genSeqData m n (Mods (o:oo))
    | debug ("»Mods "++show (o:oo))=undefined
    | otherwise = (\x->foldM f x oo)=<<r -- foldl cat (genModData m n o) oo
    where
      r=genModData m n o
      f::String->Mod->Either String String
      f (ma,ca) b= second (ca<>) <$>genModData ma n b
genSeqData m n (Seq o)
  | debug ("Seq "++show o)=undefined
  | otherwise =genModData m n o
genSeqData m n (Except r e)
  = Right (nm,do
      (MetaResult r')<-lookAhead $ option mempty (try$rp)
      (MetaResult e')<-lookAhead $ option mempty (try$ccMod m n e)
      if not (null r') && null e'
      then rp
      else unexpected (concat e'))
  where Right (nm,rp)=genModData m n r

genModData::CompExprMap->String->Mod->Either String String
genModData m n (Term o)=genTermData m n o
genModData m n (Many o)
  | debug ("Many "++show o)=undefined
  | otherwise =case genTermData m n o of
      Right (nm,ce)->Right (nm,
          do
            rs<-many (try ce)
            return$foldl (<>) mempty rs
        )
      x->x
genModData m n (Many1 o)
  | debug ("Many1 "++show o)=undefined
  | otherwise=case genTermData m n o of
      Right (nm,ce)->Right (nm,
          do
            rs<-many1 (try ce)
            return$foldl (<>) mempty rs
        )
      x->x
genModData m n (Option o)
  | debug ("Option "++show o)=undefined
  | otherwise =case genTermData m n o of
      Right (nm,ce)->Right (nm,
            option mempty ce
        )
      x->x

-- genTermData::CompExprMap->String->Terms->Either String String
-- genTermData m n t=Right (nm,do
--       mr@(MetaResult r' d)<-r
--       if length ("debug:"§d)>3
--         then error $concat d
--         else return (mr%(show(length d)++": ")))
--   where Right (nm,r)=genTermData' m n t

genTermData=genTermData'

genTermData'::CompExprMap->String->Terms->Either String String
genTermData' m n (Text o)=Right (m,do{s<-string o;return $MetaResult [s]})
genTermData' m n (Ch o)=Right (m,do{c<-char(toChar o);return $MetaResult [[c]]})
genTermData' m n (OneOf o)
  =Right (m,do
    c<-satisfy(genChars o)
    return $MetaResult [[c]])
genTermData' m n (NoneOf o)
  =Right (m,do
    c<-satisfy(not . genChars o)
    return $MetaResult [[c]])
genTermData' m n (Ref (Data rn))=genData' m rn
genTermData' m n (Ref (Symbol rn))= genData' m rn
genTermData' m n (Parens e)=genExprData m n e

-- cc :: Either String (t, t1) -> t1
-- cc (Right (_,x))=x
-- cc (Left m)= error m
-- ccExpr :: CompExprMap -> String -> Expr -> String
-- ccExpr m n o=cc (genExprData m n o)
-- ccSeq :: CompExprMap -> String -> Seq -> String
-- ccSeq m n o=cc (genSeqData m n o)
-- ccMod :: CompExprMap -> String -> Mod -> String
-- ccMod m n o=cc (genModData m n o)
-- ccTerm :: CompExprMap -> String -> Terms -> String
-- ccTerm m n o=cc (genTermData m n o)

genRange::IsChar o=>Range o->Char->String
genRange Range{..} c="genRange Range{..} c\n"
genSet::IsChar o=>Set o->Char->String
genSet (ChSeq o) c="genSet (ChSeq o) c\n"
genSet (Ranges o) c ="genSet (Ranges o) c\n"
genChars::Chars->Char->String
genChars (HexSet o) c="genChars (HexSet o) c\n"
genChars (CharSet o) c="genChars (CharSet o) c\n"

genMap::EBNF->CompExprMap
genMap EBNF{..}=Map.fromList $ map (\Rule{..}->(getName _name,Right _expr)) _bnf
