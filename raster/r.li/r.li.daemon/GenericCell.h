 
#include <grass/gis.h>
#include <grass/glocale.h>

#ifndef GENERICCELL_H
#define GENERICCELL_H

#define HIGHER 1
#define EQUAL 2
#define LOWER 3
#define DIFFERENT_TYPE 0
#define ERR_UNKNOWN -1

typedef union cella
{
    CELL c;
    DCELL dc;
    FCELL fc;
}cella;

typedef struct generic_cell
{
    int t;
    cella val;
}generic_cell;

#endif

void printGenericCell(generic_cell c);
int equalsGenericCell (generic_cell c1, generic_cell c2);






