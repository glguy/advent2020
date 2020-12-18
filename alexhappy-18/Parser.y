{
module Parser where

import Lexer (Token(..))
}

%tokentype                      { Token       }

%token
INTEGER                         { TInteger $$ }
'('                             { TOpen       }
')'                             { TClose      }
'*'                             { TMul        }
'+'                             { TAdd        }

%name expr1 expr1
%name expr2 expr2

%error                          { error "bad parse" }

%%

aexpr(top) ::                   { Expr       }
  : INTEGER                     { Integer $1 }
  | '(' top ')'                 { $2         }

expr1 ::                        { Expr       }
  : expr1 '+' aexpr(expr1)      { Add $1 $3  }
  | expr1 '*' aexpr(expr1)      { Mul $1 $3  }
  |           aexpr(expr1)      { $1         }

expr2_ ::                       { Expr       }
  : expr2_ '+' aexpr(expr2)     { Add $1 $3  }
  |            aexpr(expr2)     { $1         }

expr2 ::                        { Expr       }
  : expr2 '*' expr2_            { Mul $1 $3  }
  |           expr2_            { $1         }

{
data Expr = Integer Integer | Add Expr Expr | Mul Expr Expr deriving Show
}
