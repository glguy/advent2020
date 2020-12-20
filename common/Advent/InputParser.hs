{-# Language TemplateHaskell #-}
module Advent.InputParser (format, makeParser) where

import Control.Applicative (some)
import Advent.InputParser.Parser
import Advent.InputParser.Lexer
import Advent.InputParser.Syntax
import Text.ParserCombinators.ReadP
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.Maybe

parse :: String -> Syntax
parse = inputParser . alexScanTokens

days =
  map parse
  [ "(%u%n)*"
  , "(%u-%u: %s%n)*"
  , "(%s:%s( |%n))+&%n"
  , "(%s %s bags contain (no other bags|((%u %s %s bag(|s))&(, ))).%n)*"
  ]

format :: QuasiQuoter
format = QuasiQuoter
  { quoteExp  = makeParser . cleanIndent
  , quotePat  = \_ -> fail "format: patterns not supported"
  , quoteType = \_ -> fail "format: types not supported"
  , quoteDec  = \_ -> fail "format: declarations not supported"
  }

cleanIndent :: String -> String
cleanIndent str =
  case lines str of
    x:xs | all (' '==) x -> concatMap (drop n) xs
      where
        n = minimum (map (length . takeWhile (' '==)) xs)
    _ -> str

makeParser :: String -> ExpQ
makeParser str = [| \inp -> maybe (error "bad input parse") fst (listToMaybe (readP_to_S ($(toReadP (parse str)) <* eof) inp)) |]

toReadP :: Syntax -> ExpQ
toReadP s =
  case s of
    Literal c -> [| () <$ char c |]

    Unsigned  -> [| (read :: String -> Integer) <$> some (satisfy isDigit) |]
    Signed    -> [| (read :: String -> Integer) <$> ((++) <$> option "" (string "-") <*> some (satisfy isDigit)) |]
    Char      -> [| get |]
    Word      -> [| some (satisfy (not . isSpace)) |]
    Empty     -> [| pure () |]

    Many x
      | interesting x -> [| many $(toReadP x) |]
      | otherwise     -> [| () <$ many $(toReadP x) |]

    Some x
      | interesting x -> [| some $(toReadP x) |]
      | otherwise     -> [| () <$ some $(toReadP x) |]

    SepBy x y
      | interesting x -> [| sepBy $(toReadP x) $(toReadP y) |]
      | otherwise     -> [| () <$ sepBy $(toReadP x) $(toReadP y) |]

    Alt x y
      | xn, yn -> [| (Left    <$> $(toReadP x)) +++ (Right   <$> $(toReadP y)) |]
      | xn     -> [| (Just    <$> $(toReadP x)) +++ (Nothing <$  $(toReadP y)) |]
      |     yn -> [| (Nothing <$  $(toReadP x)) +++ (Just    <$> $(toReadP y)) |]
      | otherwise -> [|           $(toReadP x) +++               $(toReadP y) |]
      where
        xn = interesting x
        yn = interesting y

    Seq x y
      | n > 1     -> foldl (\l r -> if interesting r then [| $l <*> $(toReadP r) |]
                                                     else [| $l <*  $(toReadP r) |]
                           ) [| pure $(conE (tupleDataName n)) |] xs
      | n == 1    -> foldl (\l r -> if interesting r then [| $l  *> $(toReadP r) |]
                                                     else [| $l <*  $(toReadP r) |]
                           ) (toReadP (head xs)) (tail xs)
      | otherwise -> foldl (\l r -> [| $l *> $r |]) (toReadP (head xs)) (map toReadP (tail xs))
      where
        xs = seqs (Seq x y)
        n  = length (filter interesting xs)

seqs :: Syntax -> [Syntax]
seqs (Seq x y) = seqs x ++ [y]
seqs x         = [x]
