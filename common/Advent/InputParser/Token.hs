module Advent.InputParser.Token where

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
  | TNewline
  | TMany
  | TSome
  | TSepBy
  | TAlt
  | TBang
  | TLiteral Char
  deriving (Eq, Ord, Show, Read)
