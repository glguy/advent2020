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
"%n"    { const TNewline                }
"*"     { const TMany                   }
"+"     { const TSome                   }
"&"     { const TSepBy                  }
"|"     { const TAlt                    }
"!"     { const TBang                   }
"%" .   { TLiteral . head . tail        }
.       { TLiteral . head               }

{
}
