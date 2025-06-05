
#include "TablaTipos.h"

TablaTipos::TablaTipos()
{
  // inicializar con los tipos básicos
  
  unTipo b;
  
  b.clase = TIPOBASICO;
  b.tipoBase = ENTERO;  // por si acaso, aunque no se debe usar ENTERO==0 == posición en el vector 'tipos'
  b.tamano = 1;
  tipos.push_back(b);
  
  b.tipoBase = REAL;  // tampoco se usa
  tipos.push_back(b);
}

unsigned TablaTipos::nuevoTipoArray(unsigned tam,unsigned tbase)
{
  unTipo a;

  a.clase = ARRAY;
  a.tamano = tam;
  a.tipoBase = tbase;

  tipos.push_back(a);
  return tipos.size()-1;
}

