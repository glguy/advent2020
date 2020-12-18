{
module Lexer where
}
%wrapper "basic"

tokens :-

$white+;
[0-9]+ { TInteger . read }
"*"    { const TMul      }
"+"    { const TAdd      }
"("    { const TOpen     }
")"    { const TClose    }

{
data Token = TInteger Integer | TAdd | TMul | TOpen | TClose deriving Show
}
