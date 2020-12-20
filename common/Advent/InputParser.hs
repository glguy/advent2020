{-# Language TemplateHaskell #-}
module Advent.InputParser (format) where

import Advent (getRawInput)
import Control.Applicative ((<|>), some)
import Control.Monad
import Advent.InputParser.Parser
import Advent.InputParser.Lexer
import Advent.InputParser.Syntax
import Text.ParserCombinators.ReadP
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char
import Data.Maybe
import Text.Read (readMaybe)

parse :: String -> Syntax
parse = inputParser . alexScanTokens

format :: QuasiQuoter
format = QuasiQuoter
  { quoteExp  = uncurry makeParser <=< prepare
  , quotePat  = \_ -> fail "format: patterns not supported"
  , quoteType = \_ -> fail "format: types not supported"
  , quoteDec  = \_ -> fail "format: declarations not supported"
  }

prepare :: String -> Q (Int,String)
prepare str =
  case lines str of
    []   -> fail "Empty input format"
    [x]  -> case reads x of
              [(n,rest)] -> pure (n, dropWhile (' '==) rest)
              _ -> fail "Failed to parse single-line input pattern"
    x:xs ->
      do n <- case readMaybe x of
                Nothing -> fail "Failed to parse format day number"
                Just n  -> pure n
         pure (n, concatMap (drop indent) xs1)
      where
        xs1    = filter (any (' ' /=)) xs
        indent = minimum (map (length . takeWhile (' '==)) xs1)

makeParser :: Int -> String -> ExpQ
makeParser n str =
  [| maybe (error "bad input parse") fst . listToMaybe . readP_to_S ($(toReadP (parse str)) <* eof)
     <$> getRawInput n |]

toReadP :: Syntax -> ExpQ
toReadP s =
  case s of
    Literal xs_ -> [| () <$ string xs |]
      where xs = reverse xs_

    UnsignedInteger -> [| (read :: String -> Integer) <$>                                      munch1 isDigit  |]
    SignedInteger   -> [| (read :: String -> Integer) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]
    UnsignedInt     -> [| (read :: String -> Int    ) <$>                                      munch1 isDigit  |]
    SignedInt       -> [| (read :: String -> Int    ) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]

    Char      -> [| satisfy ('\n' /=) |]
    Word      -> [| munch1 (not . isSpace) |]

    Many x
      | interesting x -> [|       many $(toReadP x) |]
      | otherwise     -> [| () <$ many $(toReadP x) |]

    Some x
      | interesting x -> [|       some $(toReadP x) |]
      | otherwise     -> [| () <$ some $(toReadP x) |]

    SepBy x y
      | interesting x -> [|       sepBy $(toReadP x) $(toReadP y) |]
      | otherwise     -> [| () <$ sepBy $(toReadP x) $(toReadP y) |]

    Alt x y
      | xi, yi    -> [| Left    <$> $(toReadP x) <|> Right   <$> $(toReadP y) |]
      | xi        -> [| Just    <$> $(toReadP x) <|> Nothing <$  $(toReadP y) |]
      |     yi    -> [| Nothing <$  $(toReadP x) <|> Just    <$> $(toReadP y) |]
      | otherwise -> [|             $(toReadP x) <|>             $(toReadP y) |]
      where
        xi = interesting x
        yi = interesting y

    Follow xs_
      | null xs   -> [| pure () |]
      | otherwise -> foldl (\l r ->
                                let r' = toReadP r in
                                if interesting r then [| $l <*> $r' |] else [| $l <* $r' |]
                           ) fun xs
      where
        xs = reverse xs_
        n  = length (filter interesting xs)

        fun
          | n == 1    = [| pure id |]
          | otherwise = [| pure $(conE (tupleDataName n)) |]
