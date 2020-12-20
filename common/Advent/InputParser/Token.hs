module Advent.InputParser.Token where

data Token
  = TOpenGroup
  | TCloseGroup
  | TAnyChar
  | TAnyWord
  | TUnsigned
  | TSigned
  | TNewline
  | TMany
  | TSome
  | TSepBy
  | TAlt
  | TLiteral Char
  deriving (Eq, Ord, Show, Read)
