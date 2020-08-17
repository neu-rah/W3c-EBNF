-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module W3cData where

-- import qualified Data.Map as Map
-- import Text.ParserCombinators.Parsec (Parser)

-- W3c EBNF AST
-- "making illegal states irrepresentable"
-- https://www.w3.org/TR/REC-xml/#sec-notation

data EBNF=EBNF {_entry::String,_bnf::[Symbol]} deriving (Show,Eq) -- [Symbol]
data Symbol=Rule{_name::Name, _expr::Expr} deriving (Show,Eq)
data Expr=Alt [Seq]|Expr Seq deriving (Show,Eq)
data Seq = Mods [Mod] | Seq Mod | Except Mod Mod deriving (Show,Eq)

data Mod
  = Term Terms
  | Many Terms
  | Many1 Terms
  | Option Terms
  deriving (Show,Eq)

data Terms
  = Text String
  | Ch Code
  | OneOf Chars
  | NoneOf Chars
  | Ref Name
  | Parens Expr
  deriving (Show,Eq)

data Name = Data String | Symbol String deriving (Show,Eq)
newtype Code=Code String deriving (Show,Eq)
data Range o=Range{_from::o, _to::o}
data Set o=ChSeq[o]|Ranges[Range o]
data Chars
  =CharSet (Set Char)
  |HexSet (Set Code)
  deriving (Show,Eq)

class IsChar o where
  toChar::o->Char

instance IsChar Char where
  toChar=id

instance IsChar Code where
  toChar=toEnum . fromHex
    where
      hexDigit::Char->Int
      hexDigit '0'=0
      hexDigit '1'=1
      hexDigit '2'=2
      hexDigit '3'=3
      hexDigit '4'=4
      hexDigit '5'=5
      hexDigit '6'=6
      hexDigit '7'=7
      hexDigit '8'=8
      hexDigit '9'=9
      hexDigit 'A'=10
      hexDigit 'B'=11
      hexDigit 'C'=12
      hexDigit 'D'=13
      hexDigit 'E'=14
      hexDigit 'F'=15
      hexDigit 'a'=10
      hexDigit 'b'=11
      hexDigit 'c'=12
      hexDigit 'd'=13
      hexDigit 'e'=14
      hexDigit 'f'=15
      hexDigit  x = error $ "invalid hexadecimal digit '"++[x]++"'"
      fromHex::Code->Int
      fromHex (Code o)=tc 0 o
        where
          tc::Int->String->Int
          tc = foldl (\ n o -> 16 * n + hexDigit o)

instance Show o=> Show (Range o) where
  show (Range f t)="Range{_from="++show f++",_to="++show t++"}"
instance Show o=> Show (Set o) where
  show (ChSeq o)="ChSeq "++show o
  show (Ranges o)="(Ranges"++show o++")"
instance Eq o=>Eq (Range o) where
  (Range a b) == (Range c d)=a==c&&b==d
instance Eq o=>Eq (Set o) where
  (ChSeq a)==(ChSeq b)=a==b
  (Ranges a)==(Ranges b)=a==b
