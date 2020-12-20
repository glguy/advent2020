{
module Advent.InputParser.Parser where

import Advent.InputParser.Token
import Advent.InputParser.Format
}

%tokentype                      { Token                 }

%token
'('                             { TOpenGroup            }
')'                             { TCloseGroup           }
'*'                             { TMany                 }
'+'                             { TSome                 }
'&'                             { TSepBy                }
'|'                             { TAlt                  }
'%c'                            { TAnyChar              }
'%s'                            { TAnyWord              }
'%u'                            { TUnsignedInt          }
'%d'                            { TSignedInt            }
'%lu'                           { TUnsignedInteger      }
'%ld'                           { TSignedInteger        }
'%n'                            { TNewline              }
LIT                             { TLiteral $$           }

%name parseFormat

%monad                          { Either [Token]        }
%error                          { Left                  }

%left '|'
%left '&' '*' '+'

%%

format
  :                             { Follow []             }
  | atoms                       { $1                    }
  | format '|' format           { Alt $1 $3             }

atoms
  :       atom                  { $1                    }
  | atoms atom                  { follow $1 $2          }

atom
  : '(' format ')'              { $2                    }
  | '%u'                        { UnsignedInt           }
  | '%d'                        { SignedInt             }
  | '%lu'                       { UnsignedInteger       }
  | '%ld'                       { SignedInteger         }
  | '%s'                        { Word                  }
  | '%c'                        { Char                  }
  | '%n'                        { Literal "\n"          }
  | LIT                         { Literal [$1]          }
  | atom '*'                    { Many $1               }
  | atom '+'                    { Some $1               }
  | atom '&' atom               { SepBy $1 $3           }

{
}
