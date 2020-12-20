{-# Language GADTs, KindSignatures #-}
module Advent.InputParser.Syntax where

data Syntax
  -- repetitions
  = Many  Syntax
  | Some  Syntax
  | SepBy Syntax Syntax
  -- combinations
  | Alt Syntax Syntax
  | Follow [Syntax] -- REVERSE ORDER!
  -- primitives
  | Literal String -- REVERSE ORDER!
  | UnsignedInteger
  | SignedInteger
  | UnsignedInt
  | SignedInt
  | Word
  | Char
  deriving Show

interesting :: Syntax -> Bool
interesting s =
  case s of
    Many x              -> interesting x
    Some x              -> interesting x
    SepBy x _           -> interesting x
    Alt x y             -> interesting x || interesting y
    Follow xs           -> any interesting xs
    UnsignedInteger     -> True
    SignedInteger       -> True
    UnsignedInt         -> True
    SignedInt           -> True
    Word                -> True
    Char                -> True
    Literal{}           -> False

follow :: Syntax -> Syntax -> Syntax
follow (Literal x) (Literal y) = Literal (y++x)
follow (Follow (Literal x:xs)) (Literal y) = Follow (Literal (y++x):xs)
follow (Follow x) (Follow y) = Follow (y++x)
follow x          (Follow y) = Follow (y++[x])
follow (Follow x) y          = Follow (y:x)
follow x          y          = Follow [y,x]
