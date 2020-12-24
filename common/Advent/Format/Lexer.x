{
module Advent.Format.Lexer where

import Advent.Format.Types
}
%wrapper "basic"

tokens :-

"("     { const TOpenGroup              }
")"     { const TCloseGroup             }
"%c"    { const TAnyChar                }
"%a"    { const TAnyLetter              }
"%s"    { const TAnyWord                }
"%u"    { const TUnsignedInt            }
"%d"    { const TSignedInt              }
"%lu"   { const TUnsignedInteger        }
"%ld"   { const TSignedInteger          }
"*"     { const TMany                   }
"+"     { const TSome                   }
"&"     { const TSepBy                  }
"|"     { const TAlt                    }
"!"     { const TBang                   }
"@" .   { TAt . tail                    }
"%n"    { const (TLiteral '\n')         }
"%" .   { TLiteral . head . tail        }
.       { TLiteral . head               }

{
}
