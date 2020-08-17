{-# LANGUAGE CPP #-}
-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Compiler where
-- use a W3C Grammar AST and build a parser for it at runtime
-- this is damn slow, works well for simple grammars... 
-- need to generate a parser source to be compiled instead

import W3cData
import Debug
import Parser
import Utils

-- import Text.Heredoc
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)
import qualified Data.Map as Map
import Data.Functor.Identity
import Data.Monoid hiding (Alt)
import Data.Maybe
import Control.Monad
import Control.Arrow

-- compiler data
type Stat=()
type Compiled=(CompExprMap,MetaParser)
data MetaResult=MetaResult{
  _res::[String]
#ifdef DEBUG
  ,_deb::[String]
#endif
  } deriving (Show)
-- type MetaResult=[String]
type MetaParser=Parser MetaResult
type CompExpr=Either MetaParser Expr
type CompExprMap=Map.Map String CompExpr

#ifdef DEBUG
(%)::MetaResult->String->MetaResult
(%) MetaResult{..} m=MetaResult{_deb=m:_deb,..}
#endif

instance Semigroup MetaResult where
  (<>) a b = MetaResult $(_res a) <> (_res b)

instance Monoid MetaResult where
#ifdef DEBUG
  mempty=MetaResult mempty mempty
  mappend (MetaResult r1 d1)(MetaResult r2 d2)=MetaResult (r1++r2) (d1++d2)
#else
  mempty=MetaResult mempty
  mappend (MetaResult r1)(MetaResult r2)=MetaResult (r1++r2)
#endif

run::MetaParser->String->Either ParseError MetaResult
run p=runParser p () "metaparser"

compile::EBNF->Either String Compiled
compile e@EBNF{..}=compile' (mkMap e) ("compile"§_entry)

compile'::CompExprMap->String->Either String Compiled
compile' m n =Right (cm,
#ifdef DEBUG
      do
        (MetaResult r' d)<-cc$c
        return $MetaResult r' d%("["++n++"]: ")
#else
      do
        (MetaResult r')<-cc$c
        return $MetaResult r'
#endif
  ) where
    c=compileRule nm n=<<rule::Either String Compiled
    Right (cm,ce)=c
    nm=Map.insert n (Left ce) m::CompExprMap
    rule=Map.lookup n m ~> ("Rule <"++n++"> not found"::String)

compileRule::CompExprMap->String->CompExpr->Either String Compiled
compileRule m _ (Left c) = Right (m,c) -- already compiled (avoid checking infinite parsers)
compileRule m n (Right e) = compileExpr m n e -- ask for compilation

compileExpr::CompExprMap->String->Expr->Either String Compiled
compileExpr m n (Alt (o:oo))= (\x->foldM f x oo)=<<r
  where
    r=compileSeq m n o
    f::Compiled->Seq->Either String Compiled
    f (ma,ca) b=second (ca<||>)<$>compileSeq ma n b
compileExpr m n (Expr o)=compileSeq m n o

-- instance Monoid MetaParser where
--   mempty= return mempty
--   mappend a b=do
--     a'<-a
--     b'<-b
--     return (a'<>b')

compileSeq::CompExprMap->String->Seq->Either String Compiled
compileSeq m n (Mods [])=error "empty mods list"
compileSeq m n (Mods (o:oo))
    | debug ("»Mods "++show (o:oo))=undefined
    | otherwise = (\x->foldM f x oo)=<<r -- foldl cat (compileMod m n o) oo
    where
      r=compileMod m n o
      f::Compiled->Mod->Either String Compiled
      f (ma,ca) b= second (ca<>) <$>compileMod ma n b
      -- cat::Either String Compiled->Mod->Either String Compiled -- MetaParser
      -- cat a b=do
      --   a'<-a
      --   b'<-compileMod m n b
      --   return (a'<>b') -- %"mods:")
compileSeq m n (Seq o)
  | debug ("Seq "++show o)=undefined
  | otherwise =compileMod m n o
compileSeq m n (Except r e)
  = Right (nm,do
#ifdef DEBUG
      (MetaResult r' dr)<-lookAhead $ option (MetaResult []["Except "++show r++" failed"]) (try$rp)
      (MetaResult e' de)<-lookAhead $ option (MetaResult []["Except "++show e++" failed"]) (try$ccMod m n e)
#else
      (MetaResult r')<-lookAhead $ option mempty (try$rp)
      (MetaResult e')<-lookAhead $ option mempty (try$ccMod m n e)
#endif
      if not (null r') && null e'
      then rp
      else unexpected (concat e'))
  where Right (nm,rp)=compileMod m n r

compileMod::CompExprMap->String->Mod->Either String Compiled
compileMod m n (Term o)=compileTerm m n o
compileMod m n (Many o)
  | debug ("Many "++show o)=undefined
  | otherwise =case compileTerm m n o of
      Right (nm,ce)->Right (nm,
          do
            rs<-many (try ce)
#ifdef DEBUG
            let (MetaResult r d)=foldl (<>) mempty rs
            return$MetaResult r ("Many ":d)
#else
            return$foldl (<>) mempty rs
#endif
        )
      x->x
compileMod m n (Many1 o)
  | debug ("Many1 "++show o)=undefined
  | otherwise=case compileTerm m n o of
      Right (nm,ce)->Right (nm,
          do
            rs<-many1 (try ce)
#ifdef DEBUG
            let (MetaResult r d)=foldl (<>) mempty rs
            return$MetaResult r ("Many1 ":d)
#else
            return$foldl (<>) mempty rs
#endif
        )
      x->x
compileMod m n (Option o)
  | debug ("Option "++show o)=undefined
  | otherwise =case compileTerm m n o of
      Right (nm,ce)->Right (nm,
#ifdef DEBUG
          do
            (MetaResult r d)<-option (MetaResult [] ["Option "++show o++" failed."]) ce
            return $MetaResult r ("Option ":d)
#else
            option mempty ce
#endif
        )
      x->x

-- compileTerm::CompExprMap->String->Terms->Either String Compiled
-- compileTerm m n t=Right (nm,do
--       mr@(MetaResult r' d)<-r
--       if length ("debug:"§d)>3
--         then error $concat d
--         else return (mr%(show(length d)++": ")))
--   where Right (nm,r)=compileTerm' m n t

compileTerm=compileTerm'

compileTerm'::CompExprMap->String->Terms->Either String Compiled
compileTerm' m n (Text o)=Right (m,do{s<-string o;return $MetaResult [s]
#ifdef DEBUG
  [n++" Text "++show o]
#endif
  })
compileTerm' m n (Ch o)=Right (m,do{c<-char(toChar o);return $MetaResult [[c]]
#ifdef DEBUG
  [n++" Ch "++show o]
#endif
  })
compileTerm' m n (OneOf o)
  =Right (m,do
    c<-satisfy(chkChars o)
    return $MetaResult [[c]]
#ifdef DEBUG
      [n++" OneOf "++show o]
#endif
      )
compileTerm' m n (NoneOf o)
  =Right (m,do
    c<-satisfy(not . chkChars o)
    return $MetaResult [[c]]
#ifdef DEBUG
      [n++" NoneOf "++show o]
#endif
      )
compileTerm' m n (Ref (Data rn))=compile' m rn
compileTerm' m n (Ref (Symbol rn))= compile' m rn
compileTerm' m n (Parens e)=compileExpr m n e

cc :: Either String (t, t1) -> t1
cc (Right (_,x))=x
cc (Left m)= error m
ccExpr :: CompExprMap -> String -> Expr -> MetaParser
ccExpr m n o=cc (compileExpr m n o)
ccSeq :: CompExprMap -> String -> Seq -> MetaParser
ccSeq m n o=cc (compileSeq m n o)
ccMod :: CompExprMap -> String -> Mod -> MetaParser
ccMod m n o=cc (compileMod m n o)
ccTerm :: CompExprMap -> String -> Terms -> MetaParser
ccTerm m n o=cc (compileTerm m n o)

class ConcAlt o where
  (<||>)::o->o->o

instance ConcAlt Compiled where
  (<||>) (ma,ea) (mb,eb)=(ma<>mb,ea<>eb)

instance ConcAlt MetaParser where
  (<||>)=(</>)

(</>)::MetaParser->MetaParser->MetaParser
(</>) a b
  | debug (show a++"</>"++show b)=undefined
  | otherwise = do
#ifdef DEBUG
    (MetaResult a' _)<-lookAhead$option mempty $try a
    (MetaResult b' _)<-lookAhead$option mempty $try b
#else
    (MetaResult a')<-lookAhead$option mempty $try a
    (MetaResult b')<-lookAhead$option mempty $try b
#endif
    if null (concat a') && null (concat b') then try a<|>try b
    else
      if length (concat a') >= length (concat b')
        then a
        else b

-- strip out the compiled expression from the result...
instance Show MetaParser where
  show=const "MetaParser" -- just for a comodity, making composed datatypes also showable

chkRange::IsChar o=>Range o->Char->Bool
chkRange Range{..} c=toChar _from<=c && c<=toChar _to
chkSet::IsChar o=>Set o->Char->Bool
chkSet (ChSeq o) c=c `elem` map toChar o
chkSet (Ranges o) c =or$map (`chkRange` c) o
chkChars::Chars->Char->Bool
chkChars (HexSet o) c=chkSet o c
chkChars (CharSet o) c=chkSet o c

mkMap::EBNF->CompExprMap
mkMap EBNF{..}=Map.fromList $ map (\Rule{..}->(getName _name,Right _expr)) _bnf

-- ==================================================

-- optimize::Expr->Maybe Expr
optimize (EBNF n oo)=EBNF n $map (\x->fromMaybe x $ optRule x) oo
  where
    optRule::Symbol->Maybe Symbol
    optRule Rule{..}=Rule _name <$>optExpr _expr
    optExpr (Alt (o:oo))=Expr <$>foldM optSeq o oo
    optExpr _=Nothing
    optSeq (Seq a) (Seq b)=Seq<$>optMod a b
    optSeq _ _=Nothing
    optMod (Term a) (Term b)=Term<$>optTerm a b
    optMod _ _=Nothing
    optTerm (OneOf (HexSet (Ranges a))) (OneOf (HexSet (Ranges b)))=Just$(OneOf (HexSet (Ranges (a++b))))
    optTerm _ _=Nothing
