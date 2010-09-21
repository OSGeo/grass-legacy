/* ransurf.h                                                            */

#include <stdio.h>
#include <math.h>
#include <grass/gis.h>
#include "flag.h"

#define ODD(a)	((a) & 1)

#define SEED_MAX		54772
#define SEED_MIN		0
#define PI       		M_PI

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define CELLSORTER struct cell_sorter_
CELLSORTER {
    int R, C;
    double Value;
};

GLOBAL double NS, EW;
GLOBAL int CellCount, Rs, Cs;
GLOBAL double MaxDist, MaxDistSq;
GLOBAL FLAG *Cells;
GLOBAL CELLSORTER *DoNext;
GLOBAL CELL **Out, *CellBuffer;
GLOBAL int Seed, OutFD;
GLOBAL struct Flag *Verbose;
GLOBAL struct Option *Distance;
GLOBAL struct Option *Output;
