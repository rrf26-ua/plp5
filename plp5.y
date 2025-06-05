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

/* Variables auxiliares para el manejo de indices en arrays */
static unsigned maxIndices = 0;      /* numero de dimensiones del array */
static unsigned posIndice  = 0;      /* indice actualmente procesado */
static bool     ignoraInd  = false;  /* true si se ignoran errores del indice */
static Simbolo* currentArray = nullptr;

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

S       : FN ID PARI PARD Programa ENDFN     { printf("%s",$5.cod.c_str()); }
        ;

Programa: LInstr { $$ = $1; }
        ;

/* Bloques con pila para restaurar dirVar */
Bloque  : BLQ           { pilaDir.push_back(dirVar); ts = new TablaSimbolos(ts); }
             LInstr
          FBLQ          { dirVar = pilaDir.back(); pilaDir.pop_back();
                          ts = ts->getPadre(); $$.cod = $3.cod; }
        ;

LInstr  : LInstr PYC Instr { $$=$1; $$.cod += $3.cod; liberaTemporales(); }
        | LInstr PYC       { $$=$1; }
        | Instr            { $$=$1; }
        ;

Instr   : Declar | Asig | Print | Read | While | If | Loop | Bloque ;

/* Declaraciones */
Declar  : VAR ID OptType {
                           bool rep=false;
                           for(auto &simb: ts->simbolos)
                               if(simb.nombre==string($2.lexema)) rep=true;
                           if(rep)
                              errorSemantico(ERR_YADECL,$2.nlin,$2.ncol,$2.lexema.c_str());
                           unsigned tdecl = $3.tipo;
                           unsigned tam = (tdecl<TIPO_ARRAY)?1:tt.tipos[tdecl].tamano;
                           if(dirVar+tam-1>15999)
                              errorSemantico(ERR_NOCABE,$2.nlin,$2.ncol,$2.lexema.c_str());
                           Simbolo s{string($2.lexema),tdecl,(unsigned)dirVar,tam};
                           s.dims = $3.dims;
                           ts->newSymb(s);  dirVar += tam;
                           $$.cod = "";
                         }
        ;

OptType : DOSP Tipo { $$.tipo = $2.tipo; $$.dims = $2.dims; }
        |            { $$.tipo = ENTERO;  $$.dims.clear(); }
        ;

Tipo    : INT_T                 { $$.tipo = ENTERO; $$.dims.clear(); }
        | REAL_T                { $$.tipo = REAL;   $$.dims.clear(); }
        | ARRAY INT_T Dim       { $$.tipo = tt.nuevoTipoArray($3.dims,ENTERO); $$.dims = $3.dims; }
        | ARRAY REAL_T Dim      { $$.tipo = tt.nuevoTipoArray($3.dims,REAL);   $$.dims = $3.dims; }
        ;

Dim     : NUMINT COMA Dim {
                             if(atoi($1.lexema.c_str())<=0)
                                 errorSemantico(ERR_DIM,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.dims = $3.dims;
                             $$.dims.insert($$.dims.begin(),atoi($1.lexema.c_str()));
                           }
        | NUMINT {
                             if(atoi($1.lexema.c_str())<=0)
                                 errorSemantico(ERR_DIM,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.dims.clear();
                             $$.dims.push_back(atoi($1.lexema.c_str()));
                           }
        ;

/* Asignación y E/S */
Asig    : LET Ref ASIG Expr {
                             if(!$2.lvalor)                      errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             if($2.tipo==ENTERO && $4.tipo==REAL) errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             string rhs = $4.cod;
                             if($2.tipo==REAL && $4.tipo==ENTERO) rhs += "itor\n";
                             string almacen;
                             if(!$2.dims.empty()){
                                 almacen = $2.cod + rhs + "mov " + to_string($2.dir) + " A\nmov A @A\n";
                             }else{
                                 almacen = $2.cod + rhs + "mov A @B+"+to_string($2.dir)+"\n";
                             }
                             $$.cod = almacen;
                           }
        ;

Print   : PRINT Expr { string ins = ($2.tipo==ENTERO) ? "wri " : "wrr ";
                       $$.cod = $2.cod + ins + "@A\nwrc #10\n"; }
        ;

Read    : READ Ref   {
                        string ins = ($2.tipo==ENTERO) ? "rdi " : "rdr ";
                        if(!$2.dims.empty())
                           $$.cod = $2.cod + ins + "@A\nmov A " + to_string($2.dir) + " A\nmov A @A\n";
                        else
                           $$.cod = ins + "@A\nmov A @B+" + to_string($2.dir) + "\n";
                     }
        ;

/* Control */
While   : WHILE Expr Instr {
                                   if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"while");
                                   string l1=etq(), l2=etq();
                                   $$.cod = l1+": "+$2.cod+"jz "+l2+"\n"+$3.cod+"jmp "+l1+"\n"+l2+":\n";
                                 }
        ;

If      : IF Expr Instr Ip {
                                  if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"if");
                                  string l1=etq(), l2=etq();
                                  $$.cod = $2.cod + "jz " + l1 + "\n" +
                                           $3.cod + "jmp " + l2 + "\n" +
                                           l1 + ": " + $4.cod + l2 + ":\n";
                                }
        ;

Ip      : ELSE Instr FI                  { $$ = $2; }
        | ELIF Expr Instr Ip {
                                  if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"elif");
                                  string l1=etq(), l2=etq();
                                  $$.cod = $2.cod + "jz " + l1 + "\n" +
                                           $3.cod + "jmp " + l2 + "\n" +
                                           l1 + ": " + $4.cod + l2 + ":\n";
                                }
        | FI                               { $$.cod = ""; }
        |                                  { $$.cod = ""; }
        ;

/* Loop simplificado */
Loop    : LOOP ID RANGE Rango LInstr ENDLOOP {
                                   Simbolo *s = ts->searchSymb(string($2.lexema));
                                   if(!s || s->tipo!=ENTERO)
                                         errorSemantico(ERR_LOOP,$1.nlin,$1.ncol,$2.lexema.c_str());
                                   int tmp = nuevaTemp();
                                   string l1=etq(), l2=etq();
                                   $$.cod = "mov #"+to_string($4.tpos)+" "+to_string(tmp)+"\n"+
                                            l1+": mov "+to_string(tmp)+" A\n"+
                                            "mov A @B+"+to_string(s->dir)+"\n"+
                                            $5.cod+"inc "+to_string(tmp)+"\ncmp #"+to_string($4.dir)+"\njnz "+l1+"\n"+l2+":\n";
                                 }
        ;
Rango   : NUMINT DOSP NUMINT { $$.tpos = atoi($1.lexema.c_str()); $$.dir = atoi($3.lexema.c_str()); }
        | NUMINT             { $$.tpos = 0; $$.dir = atoi($1.lexema.c_str()); }
        ;
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
                    if(!s){ if(!ignoraInd) errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.cod=""; $$.tipo=ENTERO; $$.dir=0; $$.lvalor=false; $$.dims.clear(); }
                    else {
                       $$.cod="mov @B+"+to_string(s->dir)+" A\n";
                       $$.tipo=s->tipo; $$.dir=s->dir; $$.lvalor=true;
                       $$.dims = s->dims;
                    }
                  }
        | ID { currentArray = ts->searchSymb(string($1.lexema));
                if(!currentArray) errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str()); }
          CORA { maxIndices=currentArray->dims.size(); posIndice=0; }
          ExprLista CORD {
                    Simbolo *s = currentArray;
                    if(s->tipo<2) errorSemantico(ERR_SOBRAN,$3.nlin,$3.ncol,"[");
                    unsigned ndeclarados = s->dims.size();
                    unsigned nusados = $5.indices.size();
                    if(nusados<ndeclarados) errorSemantico(ERR_FALTAN,$6.nlin,$6.ncol,"]");
                    if(nusados>ndeclarados) {
                        Indice ex = $5.indices[ndeclarados];
                        errorSemantico(ERR_SOBRAN,ex.nlin,ex.ncol-2,"");
                    }
                    string code="";
                    int tempOff = nuevaTemp();
                    for(size_t i=0;i<ndeclarados;i++){
                        Indice idx = $5.indices[i];
                        if(idx.tipo!=ENTERO) errorSemantico(ERR_INDICE_ENTERO,idx.nlin,idx.ncol-1,"");
                        code += idx.cod;
                        if(i==0){
                            code += "mov @B+"+to_string(idx.dir)+" A\n";
                            code += "mov A "+to_string(tempOff)+"\n";
                        }else{
                            code += "mov "+to_string(tempOff)+" A\n";
                            code += "muli #"+to_string(s->dims[i])+" "+to_string(tempOff)+"\n";
                            code += "mov @B+"+to_string(idx.dir)+" A\n";
                            code += "addi "+to_string(tempOff)+"\n";
                        }
                    }
                    int taddr = nuevaTemp();
                    code += "mov #"+to_string(s->dir)+" "+to_string(taddr)+"\n";
                    code += "mov "+to_string(taddr)+" A\n";
                    code += "addi "+to_string(tempOff)+"\n";
                    code += "mov A "+to_string(taddr)+"\n";
                    code += "mov @A A\n";
                    $$.cod = code;
                    $$.tipo = tt.tipos[s->tipo].tipoBase;
                    $$.dir = taddr; $$.lvalor=true; $$.dims.clear();
                  }
        ;

ExprLista : ExprLista COMA { ignoraInd = (posIndice >= maxIndices); } Expr
            { $$=$1; Indice in; if(!ignoraInd){ in.cod=$4.cod; in.tipo=$4.tipo; in.dir=$4.dir; } else { in.cod=""; in.tipo=ENTERO; in.dir=0; } in.nlin=$4.nlin; in.ncol=$4.ncol; $$.indices.push_back(in); posIndice++; ignoraInd=false; }
          | Expr                { ignoraInd = (posIndice >= maxIndices); Indice in; if(!ignoraInd){ in.cod=$1.cod; in.tipo=$1.tipo; in.dir=$1.dir; } else { in.cod=""; in.tipo=ENTERO; in.dir=0; } in.nlin=$1.nlin; in.ncol=$1.ncol; $$.indices.clear(); $$.indices.push_back(in); posIndice++; ignoraInd=false; }
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
