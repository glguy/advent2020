module Advent.InputParser.Token where

data Token
  = TOpenGroup
  | TCloseGroup
  | TAnyChar
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
  | TLiteral Char
  deriving (Eq, Ord, Show, Read)
