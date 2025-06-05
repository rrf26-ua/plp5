
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

unsigned TablaTipos::nuevoTipoArray(const vector<unsigned>& dims,unsigned tbase)
{
  unTipo a;

  a.clase = ARRAY;
  a.tamano = 1;
  for(unsigned d: dims) a.tamano *= d;
  a.tipoBase = tbase;
  a.dims = dims;

  tipos.push_back(a);
  return tipos.size()-1;
}

