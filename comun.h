/*----------------------------  comun.h  -----------------------------*/
/*-------------  Práctica 5 – Compilador PLP  (UA 2024-25)  ----------*/
/*-----------------  RAÚL RUIZ FLORES  -- 50593447W  -----------------*/

#ifndef COMUN_H
#define COMUN_H

#include <string>
using std::string;

/*====================================================================*/
/* 1. Enumeración de tipos básicos y utilidades comunes               */
/*====================================================================*/
const unsigned ENTERO = 0;
const unsigned REAL   = 1;
const unsigned TIPO_ARRAY = 2;

/*  Direccionamiento de memoria:
      – Variables usuario : 0-15999
      – Temporales        : 16000-16383 (384 temporales)  */
const int DIR_BASE_TEMP = 16000;
const int DIR_MAX_TEMP  = 16383;

/*====================================================================*/
/* 2. Estructura de atributos (YYSTYPE)                               */
/*====================================================================*/
typedef struct {
    /* Información léxica */
    string  lexema;         // texto original
    int     nlin;           // línea donde aparece
    int     ncol;           // columna donde empieza

    /* Información semántica */
    unsigned tipo;          // ENTERO, REAL o índice en TablaTipos si array
    unsigned tpos;          // posición en TablaTipos (arrays) o 0
    int      dir;           // dirección en memoria (-1 si no aplica)
    bool     lvalor;        // true  -> es l-value (se puede asignar)
                            // false -> es r-value  (constante/temporal)

    /* Código generado */
    string   cod;           // fragmento m2r para esta sub-expresión
} ATR;

#define YYSTYPE ATR     /* Para flex+bison */

/*====================================================================*/
/* 3. Errores léxicos, sintácticos y semánticos                       */
/*====================================================================*/
const int ERRLEXICO  =  1,
          ERRSINT    =  2,
          ERREOF     =  3,
          ERRLEXEOF  =  4,

          ERR_YADECL =  5,
          ERR_NODECL =  6,
          ERR_NOCABE =  7,

          ERR_IFWHILE       =  8,
          ERR_LOOP          =  9,

          ERR_DIM           = 10,
          ERR_FALTAN        = 11,
          ERR_SOBRAN        = 12,
          ERR_INDICE_ENTERO = 13,

          ERR_ASIG     = 14,
          ERR_MAXTEMP  = 15;

/*====================================================================*/
/* 4. Funciones auxiliares (prototipos)                               */
/*====================================================================*/
/* --- temporales --------------------------------------------------- */
extern int ctemp;              // declarado en plp5.y; cuenta temporales usados
int  nuevaTemp();              // → dirección nueva (ERR_MAXTEMP si se agota)
void liberaTemporales();       // ctemp = 0  (al salir de cada expresión)

/* --- gestión de errores ------------------------------------------ */
void msgError      (int nerror,int fila,int col,const char *lex);
void errorSemantico(int nerror,int fila,int col,const char *lex);

/*====================================================================*/
/* 5. Variables externas que exporta el léxico                        */
/*====================================================================*/
extern int   nlin, ncol;       // actualizados en plp5.l
extern char *yytext;           // lexema actual
extern int   findefichero;     // 1 cuando se llega a EOF en flex

#endif  /* COMUN_H */
