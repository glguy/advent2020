{-# Language GADTs, KindSignatures #-}
module Advent.InputParser.Syntax where

data Syntax
  = Literal Char
  | Many  Syntax
  | Some  Syntax
  | SepBy Syntax Syntax
  | Alt Syntax Syntax
  | Seq Syntax Syntax
  | UnsignedInteger
  | SignedInteger
  | UnsignedInt
  | SignedInt
  | Word
  | Char
  | Empty
  deriving Show

interesting :: Syntax -> Bool
interesting s =
  case s of
    Literal{}           -> False
    Many x              -> interesting x
    Some x              -> interesting x
    SepBy x _           -> interesting x
    Alt x y             -> interesting x || interesting y
    Seq x y             -> interesting x || interesting y
    UnsignedInteger     -> True
    SignedInteger       -> True
    UnsignedInt         -> True
    SignedInt           -> True
    Word                -> True
    Char                -> True
    Empty               -> False
