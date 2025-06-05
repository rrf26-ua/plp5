/*------------------------------  plp5.y  ------------------------------*/
/*--------------------  RAÚL RUIZ FLORES 50593447W --------------------*/
%{
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <sstream>
#include <vector>
using namespace std;

/* Evita colisión con constantes que ya vienen en las cabeceras de la práctica */
#define ENTERO  ENTERO_TS
#define REAL    REAL_TS
#define ARRAY   ARRAY_TS
#include "TablaSimbolos.h"
#include "TablaTipos.h"
#undef  ENTERO
#undef  REAL
#undef  ARRAY

#include "comun.h"

int yylex();
int yyerror(const char*);

TablaSimbolos *ts = new TablaSimbolos(nullptr);
TablaTipos     tt;

int  dirVar = 0;           
int  ctemp  = DIR_BASE_TEMP; 
unsigned eti = 0;
static std::vector<int> pilaDir;
inline string etq(){ stringstream s; s<<"L"<<eti++; return s.str(); }

int  nuevaTemp(){ if(ctemp>DIR_MAX_TEMP) errorSemantico(ERR_MAXTEMP,0,0,""); return ctemp++; }
void liberaTemporales(){ ctemp = DIR_BASE_TEMP; }
%}

%define api.value.type {ATR}

/* TOKENS */
%token FN ENDFN INT_T REAL_T ARRAY OF LET VAR PRINT READ IF THEN ELSE ELIF FI
%token WHILE DO OD LOOP RANGE ENDLOOP BLQ FBLQ
%token PARI PARD CORA CORD COMA PYC DOSP ASIG
%token OPAS OPMD OPREL
%token ID NUMINT NUMREAL

%left OPAS
%left OPMD
%nonassoc OPREL
%right UMINUS

%start S
%%

S       : FN ID PARI PARD Bloque ENDFN        { printf("%s",$5.cod.c_str()); }
        ;

/* Bloques con pila para restaurar dirVar */
Bloque  : BLQ           { pilaDir.push_back(dirVar); ts = new TablaSimbolos(ts); }
             LInstr
          FBLQ          { dirVar = pilaDir.back(); pilaDir.pop_back();
                          ts = ts->getPadre(); $$.cod = $3.cod; }
        ;

LInstr  : LInstr PYC Instr { $$=$1; $$.cod += $3.cod; liberaTemporales(); }
        | Instr            { $$=$1; }
        ;

Instr   : Declar | Asig | Print | Read | While | If | Loop | Bloque ;

/* Declaraciones */
Declar  : VAR ID OptType {
                           if(ts->searchSymb(string($2.lexema)))
                              errorSemantico(ERR_YADECL,$2.nlin,$2.ncol,$2.lexema.c_str());
                           unsigned tdecl = $3.tipo;
                           if(dirVar+1>15999)
                              errorSemantico(ERR_NOCABE,$2.nlin,$2.ncol,$2.lexema.c_str());
                           Simbolo s{string($2.lexema),tdecl,(unsigned)dirVar,1};
                           ts->newSymb(s);  dirVar += 1;
                           $$.cod = "";
                         }
        ;

OptType : DOSP Tipo { $$.tipo = $2.tipo; }
        |            { $$.tipo = ENTERO;  }
        ;

Tipo    : INT_T                 { $$.tipo = ENTERO; }
        | REAL_T                { $$.tipo = REAL;   }
        | ARRAY INT_T Dim       { $$.tipo = tt.nuevoTipoArray($3.tpos,ENTERO); }
        | ARRAY REAL_T Dim      { $$.tipo = tt.nuevoTipoArray($3.tpos,REAL);   }
        ;

Dim     : NUMINT COMA Dim {
                             if(atoi($1.lexema.c_str())<=0)
                                 errorSemantico(ERR_DIM,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.tpos = $3.tpos * atoi($1.lexema.c_str());
                           }
        | NUMINT {
                             if(atoi($1.lexema.c_str())<=0)
                                 errorSemantico(ERR_DIM,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.tpos = atoi($1.lexema.c_str());
                           }
        ;

/* Asignación y E/S */
Asig    : LET Ref ASIG Expr {
                             if(!$2.lvalor)                      errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             if($2.tipo==ENTERO && $4.tipo==REAL) errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             string rhs = $4.cod;
                             if($2.tipo==REAL && $4.tipo==ENTERO) rhs += "itor\n";
                             $$.cod = $4.cod + rhs + "mov A_DIRB_N "+to_string($2.dir)+"\n";
                           }
        ;

Print   : PRINT Expr { string ins = ($2.tipo==ENTERO) ? "wri " : "wrr ";
                       $$.cod = $2.cod + ins + "@A\nwrc #10\n"; }
        ;

Read    : READ Ref   { string ins = ($2.tipo==ENTERO) ? "rdi " : "rdr ";
                       $$.cod = ins + "@A\nmov A_DIRB_N " + to_string($2.dir) + "\n"; }
        ;

/* Control */
While   : WHILE Expr DO LInstr OD {
                                   if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"while");
                                   string l1=etq(), l2=etq();
                                   $$.cod = l1+":\n"+$2.cod+"jz "+l2+"\n"+$4.cod+"jmp "+l1+"\n"+l2+":\n";
                                 }
        ;

If      : IF Expr THEN LInstr Ip {
                                   if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"if");
                                   string l1=etq(), l2=etq();
                                   $$.cod = $2.cod + "jz " + l1 + "\n" +
                                            $4.cod + "jmp " + l2 + "\n" +
                                            l1 + ":\n" + $5.cod + l2 + ":\n";
                                 }
        ;

Ip      : ELSE LInstr FI                  { $$ = $2; }
        | ELIF Expr THEN LInstr Ip {
                                   if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"elif");
                                   string l1=etq(), l2=etq();
                                   $$.cod = $2.cod + "jz " + l1 + "\n" +
                                            $4.cod + "jmp " + l2 + "\n" +
                                            l1 + ":\n" + $5.cod + l2 + ":\n";
                                 }
        | FI                               { $$.cod = ""; }
        ;

/* Loop simplificado */
Loop    : LOOP ID RANGE Rango LInstr ENDLOOP {
                                   Simbolo *s = ts->searchSymb(string($2.lexema));
                                   if(!s || s->tipo!=ENTERO)
                                         errorSemantico(ERR_LOOP,$1.nlin,$1.ncol,$2.lexema.c_str());
                                   int tmp = nuevaTemp();
                                   string l1=etq(), l2=etq();
                                   $$.cod = "mov #"+$4.lexema+" "+to_string(tmp)+"\n"+l1+":\n"+
                                            "mov "+to_string(tmp)+" A\nmov A_DIRB_N "+to_string(s->dir)+"\n"+
                                            $5.cod+"inc "+to_string(tmp)+"\ncmp "+$4.lexema+"\njnz "+l1+"\n"+l2+":\n";
                                 }
        ;

Rango   : NUMINT DOSP NUMINT { $$.lexema = string($1.lexema)+":"+string($3.lexema); }
        | NUMINT             { $$.lexema = "0:"+string($1.lexema); }
        ;

/* Expresiones */
Expr    : Expr OPAS Term {
                           bool real = ($1.tipo==REAL || $3.tipo==REAL);
                           string a=$1.cod, b=$3.cod;
                           if(real){ if($1.tipo==ENTERO) a+="itor\n"; if($3.tipo==ENTERO) b+="itor\n"; }
                           string op = ($2.lexema[0]=='+') ? (real?"addr ":"addi ") : (real?"subr ":"subi ");
                           int t=nuevaTemp();
                           $$.cod = a + b + op + to_string(t) + "\n";
                           $$.tipo = real?REAL:ENTERO; $$.dir=t; $$.lvalor=false;
                         }
        | OPAS Term %prec UMINUS {
                           int t=nuevaTemp();
                           $$.cod = $2.cod + "neg " + to_string(t) + "\n";
                           $$.tipo=$2.tipo; $$.dir=t; $$.lvalor=false;
                         }
        | Term { $$=$1; }
        ;

Term    : Term OPMD Fact {
                           bool real = ($1.tipo==REAL || $3.tipo==REAL);
                           string a=$1.cod, b=$3.cod;
                           if(real){ if($1.tipo==ENTERO) a+="itor\n"; if($3.tipo==ENTERO) b+="itor\n"; }
                           string op = ($2.lexema[0]=='*') ? (real?"mulr ":"muli ") : (real?"divr ":"divi ");
                           int t=nuevaTemp();
                           $$.cod = a + b + op + to_string(t) + "\n";
                           $$.tipo = real?REAL:ENTERO; $$.dir=t; $$.lvalor=false;
                         }
        | Fact { $$=$1; }
        ;

Fact    : NUMINT  { int t=nuevaTemp(); $$.cod="mov #"+string($1.lexema)+" "+to_string(t)+"\n";
                    $$.tipo=ENTERO; $$.dir=t; $$.lvalor=false; }
        | NUMREAL { int t=nuevaTemp(); $$.cod="mov $"+string($1.lexema)+" "+to_string(t)+"\n";
                    $$.tipo=REAL; $$.dir=t; $$.lvalor=false; }
        | PARI Expr PARD { $$=$2; }
        | Ref            { $$=$1; }
        ;

Ref     : ID {
                    Simbolo *s = ts->searchSymb(string($1.lexema));
                    if(!s) errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str());
                    $$.cod="mov "+to_string(s->dir)+" @A\n";
                    $$.tipo=s->tipo; $$.dir=s->dir; $$.lvalor=true;
                  }
        | ID CORA ExprLista CORD {
                    Simbolo *s = ts->searchSymb(string($1.lexema));
                    if(!s) errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str());
                    if(s->tipo<2) errorSemantico(ERR_SOBRAN,$2.nlin,$2.ncol,"[");
                    /* faltan cálculos de offset e índices */
                    $$.cod=$3.cod+"mov A_DIRB_N "+to_string(s->dir)+"\n";
                    $$.tipo=ENTERO; $$.dir=s->dir; $$.lvalor=true;
                  }
        ;

ExprLista : ExprLista COMA Expr { $$.cod=$1.cod+$3.cod; }
          | Expr                { $$=$1; }
          ;
%%

int yyerror(const char*){
   if(findefichero) msgError(ERREOF,0,0,"");
   else msgError(ERRSINT,nlin,ncol-(int)strlen(yytext),yytext);
   return 0;
}

/*–––– IMPLEMENTACIÓN DE ERROR Y main() ––––*/
void msgError(int n,int f,int c,const char *lx)
{
   switch(n){
      case ERRLEXICO : fprintf(stderr,"Error lexico (%d,%d): caracter '%s' incorrecto\n",f,c,lx); break;
      case ERRSINT   : fprintf(stderr,"Error sintactico (%d,%d): en '%s'\n",f,c,lx);             break;
      case ERREOF    : fprintf(stderr,"Error sintactico: fin de fichero inesperado\n");          break;
      case ERRLEXEOF : fprintf(stderr,"Error lexico: fin de fichero inesperado\n");              break;
   }
   exit(1);
}

void errorSemantico(int n,int f,int c,const char *lx)
{
   fprintf(stderr,"Error semantico (%d,%d): ",f,c);
   switch(n){
      case ERR_YADECL       : fprintf(stderr,"variable '%s' ya declarada\n",lx);         break;
      case ERR_NODECL       : fprintf(stderr,"variable '%s' no declarada\n",lx);         break;
      case ERR_NOCABE       : fprintf(stderr,"la variable '%s' ya no cabe en memoria\n",lx); break;
      case ERR_IFWHILE      : fprintf(stderr,"la expresion del '%s' debe ser de tipo entero\n",lx); break;
      case ERR_LOOP         : fprintf(stderr,"la variable del '%s' debe ser de tipo entero\n",lx);  break;
      case ERR_DIM          : fprintf(stderr,"la dimension debe ser mayor que 0\n");              break;
      case ERR_FALTAN       : fprintf(stderr,"faltan indices\n");                                 break;
      case ERR_SOBRAN       : fprintf(stderr,"sobran indices\n");                                 break;
      case ERR_INDICE_ENTERO: fprintf(stderr,"el indice de un array debe ser de tipo entero\n");  break;
      case ERR_ASIG         : fprintf(stderr,"tipos incompatibles en la asignacion\n");           break;
      case ERR_MAXTEMP      : fprintf(stderr,"no hay espacio para variables temporales\n");       break;
   }
   exit(-1);
}

extern FILE *yyin;

int main(int argc,char *argv[])
{
   if(argc==2){
       yyin = fopen(argv[1],"rt");
       if(!yyin){ perror("fopen"); return 1; }
   }
   return yyparse();
}
