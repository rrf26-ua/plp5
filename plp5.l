/*------------------------------ plp5.l -------------------------------*/
/*-------------------- RAÚL RUIZ FLORES 50593447W ---------------------*/

D    [0-9]
L    [A-Za-z]
LD   [A-Za-z0-9]

%{
#include <cstring>
#include <string>
#include "comun.h"
#include "plp5.tab.h"

using namespace std;

int nlin = 1;
int ncol = 1;
int findefichero = 0;

int  ret(int token);
void msgError(int nerror,int fila,int col,const char *lex);
%}

%x COMENTARIO

%%

\n                      { nlin++; ncol = 1; }
"/*"                    { ncol += yyleng; BEGIN(COMENTARIO); }
<COMENTARIO>"*/"        { ncol += yyleng; BEGIN(INITIAL); }
<COMENTARIO>\n          { nlin++; ncol = 1; }
<COMENTARIO>.           { ncol += yyleng; }
<COMENTARIO><<EOF>>     { msgError(ERRLEXEOF,-1,-1,""); }

[ \t\r]+                { ncol += yyleng; }

"("                     { return ret(PARI); }
")"                     { return ret(PARD); }
"["                     { return ret(CORA); }
"]"                     { return ret(CORD); }
","                     { return ret(COMA); }
";"                     { return ret(PYC); }
":"                     { return ret(DOSP); }
"="                     { return ret(ASIG); }

"=="|"!="|"<="|">="|"<"|">"  { return ret(OPREL); }
"+"|"-"                      { return ret(OPAS); }
"*"|"/"                      { return ret(OPMD); }

"fn"        { return ret(FN); }
"endfn"     { return ret(ENDFN); }
"int"       { return ret(INT_T); }
"real"      { return ret(REAL_T); }
"array"     { return ret(ARRAY); }
"of"        { return ret(OF); }
"let"       { return ret(LET); }
"var"       { return ret(VAR); }
"print"     { return ret(PRINT); }
"read"      { return ret(READ); }
"if"        { return ret(IF); }
"then"      { return ret(THEN); }
"else"      { return ret(ELSE); }
"elif"      { return ret(ELIF); }
"fi"        { return ret(FI); }
"while"     { return ret(WHILE); }
"do"        { return ret(DO); }
"od"        { return ret(OD); }
"loop"      { return ret(LOOP); }
"endloop"   { return ret(ENDLOOP); }
"range"     { return ret(RANGE); }
"blq"       { return ret(BLQ); }
"fblq"      { return ret(FBLQ); }

{L}({LD})*              { return ret(ID); }
{D}+(\.{D}+)?           { return strchr(yytext,'.') ? ret(NUMREAL) : ret(NUMINT); }

.                       { msgError(ERRLEXICO,nlin,ncol,yytext); }

%%

int yywrap(void) { findefichero = 1; return 1; }

int ret(int token)
{
    yylval.lexema = string(yytext);
    yylval.nlin   = nlin;
    yylval.ncol   = ncol;
    yylval.tipo   = (token==NUMINT)?ENTERO:(token==NUMREAL?REAL:0);
    yylval.tpos   = 0;
    yylval.dir    = -1;
    yylval.lvalor = false;
    yylval.cod.clear();
    ncol += yyleng;
    return token;
}
