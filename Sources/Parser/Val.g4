grammar Val;

file
  : statement* EOF
  ;

codeBlock
  : '{' statement* '}'
  ;

declBlock
  : '{' decl* '}'
  ;

statement
  : decl ';'?
  | ctrl ';'?
  | expr ';'?
  ;

decl
  : patternBindingDecl
  | funDecl
  | typeDecl
  ;

declModifierList
  : declModifier+
  ;

declModifier
  : 'mut' | 'static' | 'moveonly'
  ;

patternBindingDecl
  : varDeclKeyword pattern (':' typeRepr)? ('=' expr)?
  ;

varDeclKeyword
  : 'val' | 'var'
  ;

funDecl
  : declModifierList? funDeclKeyword funName? '(' paramList? ')' funRetAnnot? codeBlock?
  ;

funDeclKeyword
  : 'fun' | 'new'
  ;

funName
  : ident
  | overridableOper
  ;

paramList
  : param (',' param)*
  ;

param
  : paramExtName? NAME (':' typeRepr)?
  ;

paramExtName
  : NAME | '_'
  ;

funRetAnnot
  : '->' typeRepr
  ;

typeDecl
  : typeDeclKeyword NAME viewConfClause? declBlock
  ;

typeDeclKeyword
  : 'type' | 'view'
  ;

viewConfClause
  : ':' identTypeRepr (',' identTypeRepr)*
  ;

ctrl
  : retStmt
  ;

retStmt
  : 'ret' expr?
  ;

pattern
  : namedPattern
  | tuplePattern
  | wildcardPattern
  ;

namedPattern
  : NAME
  ;

tuplePattern
  : '(' tuplePatternElemList? ')'
  ;

tuplePatternElemList
  : tuplePatternElem (',' tuplePatternElem)*
  ;

tuplePatternElem
  : (NAME ':')? pattern
  ;

wildcardPattern
  : '-'
  ;

typeRepr
  : identTypeRepr
  ;

identTypeRepr
  : NAME ('::' NAME)?
  ;

expr
  : preExpr binExpr*
  ;

preExpr
  : prefixOper? postExpr
  ;

binExpr
  : infixOper preExpr
  ;

postExpr
  : postExpr '(' argList? ')' # callExpr
  | postExpr '.' NAME         # memberExpr
  | primary                   # primaryExpr
  ;

argList
  : arg (',' arg)*
  ;

arg
  : (NAME ':')? expr
  ;

primary
  : integer
  | ident
  | tuple
  | wildcard
  ;

integer
  : INT
  ;

ident
  : NAME ('::' NAME)?
  ;

tuple
  : '(' tupleElemList? ')'
  ;

tupleElemList
  : tupleElem (',' tupleElem)*
  ;

tupleElem
  : (NAME ':')? expr
  ;

wildcard
  : '_'
  ;

prefixOper
  : '+'  | '-'  | '!'  | '~'  | '&'
  ;

infixOper
  : '='
  | '+'  | '-'  | '*'  | '/'  | '%'
  | '<'  | '>'  | '<=' | '>=' | '==' | '!='
  | '&&' | '||' | '~'  | '&'  | '|'  | '^'
  ;

overridableOper
  : '='
  | '+'  | '-'  | '*'  | '/'  | '%'
  | '<'  | '>'  | '<=' | '>=' | '==' | '!='
  | '&&' | '||' | '~'  | '&'  | '|'  | '^'
  | '!'
  ;

NAME
  : [a-zA-Z_][a-zA-Z_0-9]*
  ;

INT
  : [0-9] [0-9_]*
  ;

LINE_COMMENT
  : '//' ~[\r\n]* -> channel(HIDDEN)
  ;

BLOCK_COMMENT
  : '/*' .*? '*/' -> channel(HIDDEN)
  ;

WHITESPACE
  : [ \n\r\t\u000B\u000C\u0000]+ -> skip
  ;

SHEBANG
  : '#' '!' ~('\n'|'\r')* -> channel(HIDDEN)
  ;
