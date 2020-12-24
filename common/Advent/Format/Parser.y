{
module Advent.Format.Parser where

import Advent.Format.Types
}

%tokentype                      { Token                 }

%token
'('                             { TOpenGroup            }
')'                             { TCloseGroup           }
'*'                             { TMany                 }
'+'                             { TSome                 }
'&'                             { TSepBy                }
'|'                             { TAlt                  }
'!'                             { TBang                 }
'%a'                            { TAnyLetter            }
'%c'                            { TAnyChar              }
'%s'                            { TAnyWord              }
'%u'                            { TUnsignedInt          }
'%d'                            { TSignedInt            }
'%lu'                           { TUnsignedInteger      }
'%ld'                           { TSignedInteger        }
LIT                             { TLiteral $$           }
NAME                            { TAt $$                }

%name parseFormat format

%monad                          { Either [Token]        }
%error                          { Left                  }

%left '&' '*' '+' '!'

%%

format
  : atoms                       { $1                    }
  | format '|' atoms            { Alt $1 $3             }

atoms
  :                             { Follow []             }
  | atoms atom                  { follow $1 $2          }

atom
  : '(' format ')'              { $2                    }
  | '%u'                        { UnsignedInt           }
  | '%d'                        { SignedInt             }
  | '%lu'                       { UnsignedInteger       }
  | '%ld'                       { SignedInteger         }
  | '%s'                        { Word                  }
  | '%c'                        { Char                  }
  | '%a'                        { Letter                }
  | LIT                         { Literal [$1]          }
  | atom '*'                    { Many $1               }
  | atom '+'                    { Some $1               }
  | atom '!'                    { Gather $1             }
  | atom '&' atom               { SepBy $1 $3           }
  | NAME                        { Named $1              }

{
}
