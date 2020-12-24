{-# Language BlockArguments, TemplateHaskell #-}
module Advent.Format (format) where

import Advent (count, getRawInput)
import Advent.Format.Lexer
import Advent.Format.Parser (parseFormat)
import Advent.Format.Types
import Control.Applicative ((<|>), some)
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Traversable
import Data.List (stripPrefix)
import Data.Foldable (asum)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

parse :: String -> Q Format
parse txt =
  case parseFormat (alexScanTokens txt) of
    Left e -> fail ("Format string parse failure: " ++ show e)
    Right fmt -> pure fmt

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
  do fmt <- parse str
     let formats = [| readP_to_S ($(toReadP fmt) <* eof) |]
     let qf = [| maybe (error "bad input parse") fst . listToMaybe . $formats |]
     if n == 0 then
       qf
     else
       [| $qf <$> getRawInput n |]

toReadP :: Format -> ExpQ
toReadP s =
  case s of
    Literal xs_ -> [| () <$ string xs |]
      where xs = reverse xs_

    Gather p -> [| fst <$> gather $(toReadP p) |]

    Named n
      | isUpper (head n) -> enumParser n
      | otherwise -> varE (mkName n)

    UnsignedInteger -> [| (read :: String -> Integer) <$>                                      munch1 isDigit  |]
    SignedInteger   -> [| (read :: String -> Integer) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]
    UnsignedInt     -> [| (read :: String -> Int    ) <$>                                      munch1 isDigit  |]
    SignedInt       -> [| (read :: String -> Int    ) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]

    Char      -> [| satisfy ('\n' /=) |]
    Letter    -> [| satisfy (\x -> 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z') |]
    Word      -> [| some (satisfy (not . isSpace)) |]

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
      | xi, yi    -> [| Left    <$> $xp <|> Right   <$> $yp |]
      | xi        -> [| Just    <$> $xp <|> Nothing <$  $yp |]
      |     yi    -> [| Nothing <$  $xp <|> Just    <$> $yp |]
      | otherwise -> [|             $xp <|>             $yp |]
      where
        xi = interesting x
        yi = interesting y
        xp = toReadP x
        yp = toReadP y

    Follow [] -> [| pure () |]
    Follow xs_
      | n <= 1 -> foldl (\l r ->
                            let r' = toReadP r in
                            if interesting r then [| $l *> $r' |] else [| $l <* $r' |]
                       ) (toReadP x) xs

      | interesting x ->
        foldl (\l r ->
                 let r' = toReadP r in
                 if interesting r then [| $l <*> $r' |] else [| $l <* $r' |]
              ) [| $(conE (tupleDataName n)) <$> $(toReadP x) |] xs

      | otherwise ->
        foldl (\l r ->
                 let r' = toReadP r in
                 if interesting r then [| $l <*> $r' |] else [| $l <* $r' |]
              ) [| $(conE (tupleDataName n)) <$ $(toReadP x) |] xs
      where
        x:xs = reverse xs_
        n    = Advent.count interesting (x:xs)



enumParser :: String -> ExpQ
enumParser nameStr =
  do tyName <- maybe (fail ("Failed to find type named " ++ show nameStr)) pure
           =<< lookupTypeName nameStr

     info <- reify tyName
     cons <-
       case info of
         TyConI (DataD _ _ _ _ cons _) -> pure cons
         _ -> fail ("Failed to find data declaration for " ++ show nameStr)

     entries <-
       for cons \con ->
         case con of
           NormalC name []
             | Just str <- stripPrefix nameStr (nameBase name) ->
                pure (name, str)
           _ -> fail ("Unsupported constructor: " ++ show con)

     let parsers = [[| $(conE name) <$ string str |] | (name, str) <- entries]

     [| asum $(listE parsers) |]
