{
module Advent.InputParser.Parser where

import Advent.InputParser.Token
import Advent.InputParser.Syntax
}

%tokentype                      { Token       }

%token
'('   { TOpenGroup }
')'   { TCloseGroup }
'%c'  { TAnyChar }
'%s'  { TAnyWord }
'%u'  { TUnsignedInt }
'%d'  { TSignedInt }
'%lu' { TUnsignedInteger }
'%ld' { TSignedInteger }
'%n'  { TNewline }
'*'   { TMany }
'+'   { TSome }
'&'   { TSepBy }
'|'   { TAlt }
LIT   { TLiteral $$ }

%name inputParser

%error                          { error "bad parse" }

%left '&' '*' '+'

%%

inputParser
  :                             { Literal ""            }
  | aParsers                    { $1                    }
  | inputParser '|' aParsers    { Alt $1 $3             }

aParsers
  : aParser                     { $1                    }
  | aParsers aParser            { follow $1 $2          }

aParser
  : '(' inputParser ')'         { $2                    }
  | '%u'                        { UnsignedInt           }
  | '%d'                        { SignedInt             }
  | '%lu'                       { UnsignedInteger       }
  | '%ld'                       { SignedInteger         }
  | '%s'                        { Word                  }
  | '%c'                        { Char                  }
  | '%n'                        { Literal "\n"          }
  | LIT                         { Literal [$1]          }
  | aParser '*'                 { Many $1               }
  | aParser '+'                 { Some $1               }
  | aParser '&' aParser         { SepBy $1 $3           }

{
}
