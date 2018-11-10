/****************************************************/
/* File: cminus.y                                   */
/* The CMINUS Yacc/Bison specification file         */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/
%{
#define YYPARSER /* distinguishes Yacc output from other code files */

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

#define YYSTYPE TreeNode *
static char * savedName; /* for use in assignments */
static int savedNumber;  /* for use in declarations */
static int savedLineNo;  /* ditto */
static TreeNode * savedTree; /* stores syntax tree for later return */
static int yylex(void);
%}

/* reserved words*/
%token IF ELSE INT RETURN VOID WHILE
/* multicharacter tokens */
%token ID NUM 
/* special symbols */
%token ASSIGN EQ LT PLUS MINUS TIMES OVER LPAREN RPAREN SEMI
%token NE LE GT GE LBRACE RBRACE LCURLY RCURLY COMMA
/* book-keeping tokens */
%token ERROR 

%% /* Grammar for CMINUS */

/* 1 */
program		: decl_list { savedTree = $1; }
			;
/* 2 */
decl_list	: decl_list decl 
				{ YYSTYPE t = $1;
				  if (t != NULL)
				  { while (t->sibling != NULL)
				  	  t = t->sibling;
				  	t->sibling = $2;
				    $$ = $1; }
				  else $$ = $2;
				}
			| decl { $$ = $1; }
			;
/* 3 */
decl		: var_decl { $$ = $1; }
			| fun_decl { $$ = $1; }
			;
saveName	: ID 
				{ savedName = copyString(tokenString);
				  savedLineNo = lineno;
				}
			;
saveNum		: NUM 
				{ savedNumber = atoi(tokenString);
				  savedLineNo = lineno;
				}
			;
/* 4 */
var_decl	: type_spec saveName
			  	{ $$ = newDeclNode(VarK); 
				  $$->attr.var.name = savedName;
				  $$->lineno = lineno;
				  $$->attr.var.type = $1->attr.type;
				}
			  SEMI { $$ = $3; }
			| type_spec saveName 
				{ $$ = newDeclNode(VarArrayK);
				  $$->lineno = lineno;
				  $$->attr.arr.name = savedName;
				}
			  LBRACE saveNum RBRACE SEMI 
				{ $$ = $3;
				  $$->attr.arr.size = savedNumber;
				  $$->attr.arr.type = $1->attr.type;
				}
			;
/* 5 */
type_spec	: INT 
				{ $$ = newExpNode(TypeK);
			  	  $$->attr.type = INT;
				}
			| VOID 
				{ $$ = newExpNode(TypeK);
				  $$->attr.type = VOID;
				}
			;
/* 6 */
fun_decl	: type_spec saveName 
				{ $$ = newDeclNode(FuncK);
				  $$->attr.func.name = savedName;
				  $$->lineno = lineno;
			  	} 
			  LPAREN params RPAREN comp_stmt 
				{ /*$$->attr.func.returnType = $1->attr.type;*/
				  $$ = $3;
				  /* $$->child[0] = $1; */
				  $$->attr.func.returnType = $1->attr.type;
				  $$->child[0] = $5;
				  $$->child[1] = $7;
				}
			;
/* 7 */
params		: param_list { $$ = $1; }
			| VOID		 
				{ $$ = newParamNode(SingleParamK);
				  $$->attr.var.type = VOID;
				}
			;
/* 8 */
param_list 	: param_list COMMA param 
				{ YYSTYPE t = $1;
				  if (t != NULL)
				  { while (t->sibling != NULL)
				      t = t->sibling;
				    t->sibling = $3;
				    $$ = $1; }
				  else $$ = $3;
				}
			| param { $$ = $1; }
			;
/* 9 */
param		: type_spec saveName 
				{ $$ = newParamNode(SingleParamK);
				  $$->attr.var.name = savedName;
				  $$->attr.var.type = $1->attr.type;
				  /* $$->child[0] = $1; */
				}
			| type_spec saveName LBRACE RBRACE 
				{ $$ = newParamNode(ArrParamK);
				  /* $$->child[0] = $1; */
				  $$->attr.arr.name = savedName;
				  $$->attr.arr.type = $1->attr.type;
				}
			;
/* 10 */
comp_stmt	: LCURLY local_decls stmt_list RCURLY 
				{ $$ = newStmtNode(CompK);
				  $$->child[0] = $2;
				  $$->child[1] = $3;
				}
			;
/* 11 */
local_decls	: local_decls var_decl 
				{ YYSTYPE t = $1;
				  if (t != NULL)
				  { while (t->sibling != NULL)
				  	  t = t->sibling;
				    t->sibling = $2;
				    $$ = $1; }
				  else $$ = $2;
				}
			| /* empty */	{ $$ = NULL; }
			;
/* 12 */
stmt_list	: stmt_list stmt 
				{ YYSTYPE t = $1;
				  if (t != NULL)
				  { while (t->sibling != NULL)
				      t = t->sibling;
				    t->sibling = $2;
				    $$ = $1; }
				  else $$ = $2;
				}
			| /* empty */ { $$ = NULL; }
			;
/* 13 */
stmt		: exp_stmt 	{ $$ = $1; }
			| comp_stmt { $$ = $1; }
			| sel_stmt 	{ $$ = $1; }
			| iter_stmt { $$ = $1; }
			| ret_stmt 	{ $$ = $1; }
			;
/* 14 */
exp_stmt	: exp SEMI { $$ = $1; }
			| SEMI { $$ = NULL; }
			;
/* 15 */
sel_stmt	: IF LPAREN exp RPAREN stmt 
				{ $$ = newStmtNode(IfK);
				  $$->child[0] = $3;
				  $$->child[1] = $5;
				  $$->child[2] = NULL;
				}
			| IF LPAREN exp RPAREN stmt ELSE stmt 
				{ $$ = newStmtNode(IfK);
				  $$->child[0] = $3;
				  $$->child[1] = $5;
				  $$->child[2] = $7;
				}
			;
/* 16 */
iter_stmt	: WHILE LPAREN exp RPAREN stmt 
				{ $$ = newStmtNode(IterK);
				  $$->child[0] = $3;
				  $$->child[1] = $5;
				}
			;
/* 17 */
ret_stmt	: RETURN SEMI 
				{ $$ = newStmtNode(RetK);
				  $$->child[0] = NULL;
				}
			| RETURN exp SEMI 
				{ $$ = newStmtNode(RetK);
				  $$->child[0] = $2;
				}
			;
/* 18 */
exp 		: var ASSIGN exp 
				{ $$ = newExpNode(AssignK);
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| simple_exp { $$ = $1; }
			;
/* 19 */
var			: saveName 
				{ $$ = newExpNode(IdK);
				  $$->attr.name = savedName;
				}
			| saveName 
				{ $$ = newExpNode(ArrIdK);
				  $$->attr.name = savedName; } 
			  LBRACE exp RBRACE 
				{ $$->child[0] = $3; }
			;
/* 20, 21 */ 
simple_exp 	: add_exp LE add_exp 
				{ $$ = newExpNode(OpK);
				  $$->attr.op = LE;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp LT add_exp 
				{ $$ = newExpNode(OpK);
				  $$->attr.op = LT;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp GT add_exp
				{ $$ = newExpNode(OpK);
				  $$->attr.op = GT;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp GE add_exp
				{ $$ = newExpNode(OpK);
				  $$->attr.op = GE;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp EQ add_exp
				{ $$ = newExpNode(OpK);
				  $$->attr.op = EQ;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp NE add_exp
				{ $$ = newExpNode(OpK);
				  $$->attr.op = NE;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp { $$ = $1; }
			;
/* 22, 23 */
add_exp 	: add_exp PLUS term 
				{ $$ = newExpNode(OpK);
				  $$->attr.op = PLUS;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| add_exp MINUS term 
				{ $$ = newExpNode(OpK);
				  $$->attr.op = MINUS;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| term { $$ = $1; }
			;
/* 24, 25 */ 
term		: term TIMES factor
				{ $$ = newExpNode(OpK);
				  $$->attr.op = TIMES;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| term OVER factor
				{ $$ = newExpNode(OpK);
				  $$->attr.op = OVER;
				  $$->child[0] = $1;
				  $$->child[1] = $3;
				}
			| factor { $$ = $1; }
			;
/* 26 */
factor		: LPAREN exp RPAREN { $$ = $2; }
			| var { $$ = $1; }
			| call { $$ = $1; }
			| NUM 
				{ $$ = newExpNode(ConstK);
				  $$->attr.val = atoi(tokenString);
				}
			;
/* 27 */
call		: saveName 
				{ $$ = newExpNode(CallK);
				  $$->attr.name = savedName;
				}
			  LPAREN args RPAREN 
				{ $$ = $2;
				  $$->child[0] = $4;
				}
			;
/* 28 */
args		: arg_list { $$ = $1; }
			| /* empty */ { $$ = NULL; }
			;
/* 29 */
arg_list	: arg_list COMMA exp 
				{ YYSTYPE t = $1;
				  if (t != NULL)
				  { while (t->sibling != NULL)
					  t = t->sibling;
					t->sibling = $3;
					$$ = $1; }
				  else $$ = $3;
				}
			| exp { $$ = $1; }
			;

%%

int yyerror(char * message)
{ fprintf(listing,"Syntax error at line %d: %s\n",lineno,message);
  fprintf(listing,"Current token: ");
  printToken(yychar,tokenString);
  Error = TRUE;
  return 0;
}

/* yylex calls getToken to make Yacc/Bison output
 * compatible with ealier versions of the TINY scanner
 */
static int yylex(void)
{ return getToken(); }

TreeNode * parse(void)
{ yyparse();
  return savedTree;
}

