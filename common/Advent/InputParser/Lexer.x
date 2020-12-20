{
module Advent.InputParser.Lexer where

import Advent.InputParser.Token
}
%wrapper "basic"

tokens :-

"("   { const TOpenGroup }
")"   { const TCloseGroup }
"%c"  { const TAnyChar }
"%s"  { const TAnyWord }
"%u"  { const TUnsignedInt }
"%d"  { const TSignedInt }
"%lu"  { const TUnsignedInteger }
"%ld"  { const TSignedInteger }
"%n"  { const TNewline }
"*"   { const TMany }
"+"   { const TSome }
"&"   { const TSepBy }
"|"   { const TAlt }
"%" . { TLiteral . head . tail }
.     { TLiteral . head }

{
}
