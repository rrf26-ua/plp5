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

int  dirVar = 0;                     /* siguiente dirección libre        */
int  ctemp  = DIR_BASE_TEMP;         /* siguiente temporal libre         */
unsigned eti = 0;                    /* contador de etiquetas            */
static std::vector<int> pilaDir;     /* para restaurar dirVar en bloques */

inline string etq(){ stringstream s; s<<"L"<<eti++; return s.str(); }

int  nuevaTemp(){ if(ctemp>DIR_MAX_TEMP) errorSemantico(ERR_MAXTEMP,0,0,""); return ctemp++; }
void liberaTemporales(){ /* no-op to keep temporaries unique */ }

/*--- flag para permitir identificar identificador dentro de lista de índices ---*/
static bool enIndice = false;
%}

%define api.value.type {ATR}

/* TOKENS -------------------------------------------------------------*/
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

/*--------------------------- PROGRAMA --------------------------------*/
S       : FN ID PARI PARD Programa ENDFN
        { printf("%shalt\n", $5.cod.c_str()); }
        ;

Programa: LInstr { $$ = $1; }
        ;

/*----------------------------- BLOQUES -------------------------------*/
Bloque  : BLQ           { pilaDir.push_back(dirVar); ts = new TablaSimbolos(ts); }
             LInstr
          FBLQ          { dirVar = pilaDir.back(); pilaDir.pop_back();
                          ts = ts->getPadre(); $$.cod = $3.cod; }
        ;

/*-------------------------- LISTA INSTR. -----------------------------*/
LInstr  : LInstr PYC Instr { $$=$1; $$.cod += $3.cod; liberaTemporales(); }
        | LInstr PYC       { $$=$1; }
        | Instr            { $$=$1; }
        ;

/*---------------------------- INSTRUCCIONES --------------------------*/
Instr   : Declar | Asig | Print | Read | While | If | Loop | Bloque ;

/*--------------------------- DECLARACIÓN -----------------------------*/
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
        | NUMINT                {
                             if(atoi($1.lexema.c_str())<=0)
                                 errorSemantico(ERR_DIM,$1.nlin,$1.ncol,$1.lexema.c_str());
                             $$.dims.clear();
                             $$.dims.push_back(atoi($1.lexema.c_str()));
                           }
        ;

/*------------------- ASIGNACIÓN / E/S -------------------------------*/
Asig    : LET Ref ASIG Expr {
                             if(!$2.lvalor)                      errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             if($2.tipo==ENTERO && $4.tipo==REAL) errorSemantico(ERR_ASIG,$3.nlin,$3.ncol,"=");
                             string rhs = $4.cod;
                             if($2.tipo==REAL && $4.tipo==ENTERO)
                                   rhs += "itor\nmov A " + to_string($4.dir) + "\n";

                             string almacen;
                             if(!$2.cod.empty()) /* componente de array */
                                  almacen = $2.cod + rhs +
                                            "mov " + to_string($2.dir) + " A\n"+
                                            "mov A B\n"+
                                            "mov " + to_string($4.dir) + " A\n"+
                                            "mov A @B+0\n";
                             else
                                  almacen = $2.cod + rhs +
                                            "mov #" + to_string($2.dir) + " A\n"+
                                            "mov A B\n"+
                                            "mov " + to_string($4.dir) + " A\n"+
                                            "mov A @B+0\n";
                             $$.cod = almacen;
                           }
        ;

Print   : PRINT Expr { string ins = ($2.tipo==ENTERO) ? "wri " : "wrr ";
                       $$.cod = $2.cod + ins + "A\nwrc #10\n"; }
        ;

Read    : READ Ref   {
                        string ins = ($2.tipo==ENTERO) ? "rdi " : "rdr ";
                        string code = $2.cod;
                        if(code.empty())
                               code += "mov #" + to_string($2.dir) + " A\n";
                        else
                               code += "mov " + to_string($2.dir) + " A\n";
                        code += ins + "@A\n";
                        $$.cod = code;
                     }
        ;

/*--------------------------- CONTROL --------------------------------*/
While   : WHILE Expr Instr {
                                   if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"while");
                                   string l1=etq(), l2=etq();
                                   $$.cod = l1+"\n"+$2.cod+"jz "+l2+"\n"+$3.cod+"jmp "+l1+"\n"+l2+"\n";
                                 }
        ;

If      : IF Expr Instr Ip {
                                  if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"if");
                                  string l1=etq(), l2=etq();
                                  $$.cod = $2.cod + "jz " + l1 + "\n" +
                                           $3.cod + "jmp " + l2 + "\n" +
                                           l1 + "\n" + $4.cod + l2 + "\n";
                                }
        ;

Ip      : ELSE Instr FI                  { $$ = $2; }
        | ELIF Expr Instr Ip {
                                  if($2.tipo!=ENTERO) errorSemantico(ERR_IFWHILE,$1.nlin,$1.ncol,"elif");
                                  string l1=etq(), l2=etq();
                                  $$.cod = $2.cod + "jz " + l1 + "\n" +
                                           $3.cod + "jmp " + l2 + "\n" +
                                           l1 + "\n" + $4.cod + l2 + "\n";
                                }
        | FI                               { $$.cod = ""; }
        |                                  { $$.cod = ""; }
        ;

/*---------------------------- LOOP ----------------------------------*/
Loop
  : LOOP ID RANGE Rango LInstr ENDLOOP
        {
          Simbolo *s = ts->searchSymb(string($2.lexema));
          if(!s || s->tipo!=ENTERO)
                errorSemantico(ERR_LOOP,$1.nlin,$1.ncol,$2.lexema.c_str());

          string rango = $4.lexema;
          size_t p = rango.find(':');
          string sIni = rango.substr(0,p);
          string sFin = rango.substr(p+1);

                   /* --- temporales y etiquetas --- */
          int tCont = nuevaTemp();         // contador
          int tLim  = nuevaTemp();         // límite superior
          string l0 = etq();               // cabecera
          string l1 = etq();               // salida

          /* --- generación de código --- */
          string code;
          code += "mov #"+sIni+" "+to_string(tCont)+"\n";        // i = ini
          code += "mov #"+sFin+" "+to_string(tLim )+"\n";        // lim = fin
          code += l0 + " mov " + to_string(tCont) + " A\n";      // etiqueta + A=i
          code += "gtri " + to_string(tLim) + "\n";              // i > lim ?
          code += "jnz "  + l1             + "\n";              // sí → fin
          code += "mov #" + to_string(s->dir) + " A\n";          // direccion de la variable
          code += "mov A B\n";                                   // cargar en B
          code += "mov "  + to_string(tCont) + " A\n";           // A=i
          code += "mov A @B+0\n";                               // var usuario = i
          code += $5.cod;                                        // cuerpo del bucle
          code += "mov "  + to_string(tCont) + " A\n";           // A=i
          code += "addi #1\n";                                   // A = i + 1
          code += "mov A " + to_string(tCont) + "\n";            // i = i + 1
          code += "jmp "  + l0            + "\n";               // vuelve a cabecera
          code += l1 + "\n";                                     // etiqueta de salida

          $$.cod = code;
        }
  ;

Rango   : NUMINT DOSP NUMINT   { $$.lexema = string($1.lexema)+":"+string($3.lexema); }
        | NUMINT               { $$.lexema = "0:"+string($1.lexema); }
        ;

/*------------------------- EXPRESIONES ------------------------------*/
Expr    : Expr OPAS Term {
                           bool real = ($1.tipo==REAL || $3.tipo==REAL);
                           string code = $1.cod + $3.cod;
                           if(real && $3.tipo==ENTERO){
                               code += "mov " + to_string($3.dir) + " A\n";
                               code += "itor\n";
                               code += "mov A " + to_string($3.dir) + "\n";
                           }
                           string op = ($2.lexema[0]=='+') ? (real?"addr ":"addi ") : (real?"subr ":"subi ");
                           int t=nuevaTemp();
                           code += "mov " + to_string($1.dir) + " A\n";
                           if(real && $1.tipo==ENTERO) code += "itor\n";
                           code += op + to_string($3.dir) + "\n";
                           code += "mov A " + to_string(t) + "\n";
                           $$.cod = code;
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
                           string code = $1.cod + $3.cod;
                           if(real && $3.tipo==ENTERO){
                               code += "mov " + to_string($3.dir) + " A\n";
                               code += "itor\n";
                               code += "mov A " + to_string($3.dir) + "\n";
                           }
                           string op = ($2.lexema[0]=='*') ? (real?"mulr ":"muli ") : (real?"divr ":"divi ");
                           int t=nuevaTemp();
                           code += "mov " + to_string($1.dir) + " A\n";
                           if(real && $1.tipo==ENTERO) code += "itor\n";
                           code += op + to_string($3.dir) + "\n";
                           code += "mov A " + to_string(t) + "\n";
                           $$.cod = code;
                           $$.tipo = real?REAL:ENTERO; $$.dir=t; $$.lvalor=false;
                        }
        | Fact { $$=$1; }
        ;

Fact    : NUMINT  {
                    int t=nuevaTemp();
                    $$.cod="mov #"+string($1.lexema)+" A\nmov A "+to_string(t)+"\n";
                    $$.tipo=ENTERO; $$.dir=t; $$.lvalor=false;
                  }
        | NUMREAL {
                    int t=nuevaTemp();
                    $$.cod="mov $"+string($1.lexema)+" A\nmov A "+to_string(t)+"\n";
                    $$.tipo=REAL; $$.dir=t; $$.lvalor=false;
                  }
        | PARI Expr PARD { $$=$2; }
        | Ref            {
                            if($1.lvalor){
                                int t=nuevaTemp();
                                string c = $1.cod;
                                if($1.cod.empty()){
                                     c += "mov #"+to_string($1.dir)+" A\n";
                                     c += "mov @A A\n";
                                     c += "mov A "+to_string(t)+"\n";
                                }else{
                                     c += "mov "+to_string($1.dir)+" A\n";
                                     c += "mov @A A\n";
                                     c += "mov A "+to_string(t)+"\n";
                                }
                                $$.cod=c; $$.tipo=$1.tipo; $$.dir=t; $$.lvalor=false;
                            }else
                                $$=$1;
                         }
        ;

/*----------------------- REFERENCIAS (arrays) -----------------------*/
Ref
  : ID {  /* variable simple */
        Simbolo *s = ts->searchSymb(string($1.lexema));
        if(!s){
            if(enIndice){          /* permitir para comprobar “sobran” */
                int t=nuevaTemp();
                $$.cod="mov #0 "+to_string(t)+"\n";
                $$.tipo=ENTERO; $$.dir=t; $$.lvalor=false; $$.dims.clear();
            }else
                errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str());
        }else{
                $$.cod="";               /* la direccion es conocida */
                $$.tipo=s->tipo; $$.dir=s->dir; $$.lvalor=true;
                $$.dims = s->dims;
        }
      }
  | ID CORA           { enIndice=true; }  ExprLista
    CORD              { enIndice=false;
        Simbolo *s = ts->searchSymb(string($1.lexema));
        if(!s) errorSemantico(ERR_NODECL,$1.nlin,$1.ncol,$1.lexema.c_str());
        if(s->tipo<2) errorSemantico(ERR_SOBRAN,$3.nlin,$3.ncol,"[");

        unsigned ndeclarados = s->dims.size();
        unsigned nusados     = $4.indices.size();
        if(nusados<ndeclarados) errorSemantico(ERR_FALTAN,$5.nlin,$5.ncol,"]");
        if(nusados>ndeclarados){
            Indice ex = $4.indices[ndeclarados];
            errorSemantico(ERR_SOBRAN,ex.nlin,ex.ncol,"");
        }

        /* === generación de código (idéntica) === */
        string code="";
        int tempOff = nuevaTemp();
        for(size_t i=0;i<ndeclarados;i++){
            Indice idx = $4.indices[i];
            if(idx.tipo!=ENTERO) errorSemantico(ERR_INDICE_ENTERO,idx.nlin,idx.ncol,"");
            code += idx.cod;
            if(i==0){
                code += "mov "+to_string(idx.dir)+" A\n";
                code += "mov A "+to_string(tempOff)+"\n";
            }else{
                code += "mov "+to_string(tempOff)+" A\n";
                code += "muli #"+to_string(s->dims[i])+"\n";
                code += "mov A "+to_string(tempOff)+"\n";
                code += "mov "+to_string(idx.dir)+" A\n";
                code += "addi "+to_string(tempOff)+"\n";
                code += "mov A "+to_string(tempOff)+"\n";
            }
        }
        int taddr = nuevaTemp();
        code += "mov #"+to_string(s->dir)+" "+to_string(taddr)+"\n";
        code += "mov "+to_string(taddr)+" A\n";
        code += "addi "+to_string(tempOff)+"\n";
        code += "mov A "+to_string(taddr)+"\n";

        $$.cod  = code;
        $$.tipo = tt.tipos[s->tipo].tipoBase;
        $$.dir  = taddr;
        $$.lvalor = true;
        $$.dims.clear();
      }
  ;

/*------------------------ LISTA DE EXPRESIONES ----------------------*/
ExprLista
  : ExprLista COMA Expr
      { $$ = $1;
        Indice in;
        in.cod  = $3.cod;
        in.tipo = $3.tipo;
        in.dir  = $3.dir;
        in.nlin = $2.nlin;
        in.ncol = $2.ncol;
        $$.indices.push_back(in);
      }
  | Expr
      { Indice in;
        in.cod  = $1.cod;
        in.tipo = $1.tipo;
        in.dir  = $1.dir;
        in.nlin = $1.nlin;
        in.ncol = $1.ncol;
        $$.indices.clear();
        $$.indices.push_back(in);
      }
  ;
%%

/*--------------------  Soporte C (errores y main) --------------------*/
int yyerror(const char*){
   if(findefichero) msgError(ERREOF,0,0,"");
   else msgError(ERRSINT,nlin,ncol-(int)strlen(yytext),yytext);
   return 0;
}

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