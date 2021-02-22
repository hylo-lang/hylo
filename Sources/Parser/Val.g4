grammar Val;

file
  : decl* EOF
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
  | extDecl
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
  : declModifierList? funDeclKeyword funName? genericClause? '(' funParamList? ')' funRetAnnot? codeBlock?
  ;

funDeclKeyword
  : 'fun' | 'new'
  ;

funName
  : ident
  | overridableOper
  ;

funParamList
  : funParam (',' funParam)*
  ;

funParam
  : funParamExtName? NAME (':' typeRepr)?
  ;

funParamExtName
  : NAME | '_'
  ;

funRetAnnot
  : '->' typeRepr
  ;

genericClause
  : '<' genericParamList typeReqClause? '>'
  ;

genericParamList
  : NAME (',' NAME)*
  ;

typeReqClause
  : 'where' typeReqList
  ;

typeReqList
  : typeReq (',' typeReq)*
  ;

typeReq
  : identTypeRepr '==' typeRepr     # sameTypeReq
  | identTypeRepr ':' identTypeRepr # viewConfReq
  ;

typeDecl
  : productTypeDecl
  | aliasTypeDecl
  | viewTypeDecl
  ;

productTypeDecl
  : typeDeclHead declBlock
  ;

aliasTypeDecl
  : typeDeclHead '=' typeRepr
  ;

viewTypeDecl
  : 'view' NAME inheritanceClause? declBlock
  ;

typeDeclHead
  : 'type' NAME genericClause? inheritanceClause?
  ;

extDecl
  : 'extn' identTypeRepr declBlock
  ;

  inheritanceClause
  : ':' identTypeRepr ('&' identTypeRepr)*
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
  : maxtermTypeRepr ('->' typeRepr)?
  ;

maxtermTypeRepr
  : mintermTypeRepr ('|' mintermTypeRepr)*
  ;

mintermTypeRepr
  : primaryTypeRepr ('&' primaryTypeRepr)*
  ;

primaryTypeRepr
  : identTypeRepr
  | tupleTypeRepr
  ;

identTypeRepr
  : unqualTypeRepr ('::' unqualTypeRepr)?
  ;

unqualTypeRepr
  : NAME genericArgList?
  ;

genericArgList
  : '<' typeRepr (',' typeRepr)* '>'
  ;

tupleTypeRepr
  : '(' tupleTypeElemList? ')'
  ;

tupleTypeElemList
  : tupleTypeElem (',' tupleTypeElem)*
  ;

tupleTypeElem
  : (NAME ':')? typeRepr
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
  : postExpr '(' argList? ')'   # callExpr
  | postExpr '.' NAME           # memberExpr
  | postExpr castOper typeRepr  # castExpr
  | primary                     # primaryExpr
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

castOper
  : 'as!' | 'as?' | 'as'
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
