module Advent.Format.Types where

data Token
  = TOpenGroup
  | TCloseGroup
  | TAnyChar
  | TAnyLetter
  | TAnyWord
  | TUnsignedInteger
  | TSignedInteger
  | TUnsignedInt
  | TSignedInt
  | TMany
  | TSome
  | TSepBy
  | TAlt
  | TAt String
  | TBang
  | TLiteral Char
  deriving (Eq, Ord, Show, Read)

data Format
  -- repetitions
  = Many  Format
  | Some  Format
  | SepBy Format Format
  -- combinations
  | Alt Format Format
  | Follow [Format] -- REVERSE ORDER!
  -- return matched string
  | Gather Format
  | Named String
  -- primitives
  | Literal String -- REVERSE ORDER!
  | UnsignedInteger
  | SignedInteger
  | UnsignedInt
  | SignedInt
  | Word
  | Char
  | Letter
  deriving Show

interesting :: Format -> Bool
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
    Letter              -> True
    Gather{}            -> True
    Named{}             -> True
    Literal{}           -> False

follow :: Format -> Format -> Format
follow (Literal x) (Literal y) = Literal (y++x)
follow x          (Follow []) = x
follow (Follow []) y          = y
follow (Follow (Literal x:xs)) (Literal y) = Follow (Literal (y++x):xs)
follow (Follow x) (Follow y) = Follow (y++x)
follow x          (Follow y) = Follow (y++[x])
follow (Follow x) y          = Follow (y:x)
follow x          y          = Follow [y,x]
