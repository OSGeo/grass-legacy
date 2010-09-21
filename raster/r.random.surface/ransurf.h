/* ransurf.h                                                            */
#ifndef __RANSURF_H__
#define __RANSURF_H__

#include <stdio.h>
#include <math.h>
#include <grass/gis.h>

#define ODD(a)	((a) & 1)

#define SEED_MAX		54772
#define SEED_MIN		0
#define MAX_INTERVAL		10
#define MIN_INTERVAL		-10
/* DELTA_T is the inverse of the number of subdivisions within the      */
/*      Norm[] distribution for each standard deviation.                */
/* DELTA_T == 1 / 1000                                                  */
#define DELTA_T  		0.001
/* S_O_D == (2 * MAX_INTERVAL) / DELTA_T                                */
#define SIZE_OF_DISTRIBUTION 	20000
#define PI       		M_PI

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define BIGF struct _big_f_filter_
BIGF {
    int RowPlus, ColPlus, NumR, NumC, *LowBF, *HihBF;
    double **F;
};

#define FILTER struct _filter_strteres_
FILTER {
    double Mult, MaxDist, MaxSq, Exp;
};

#define CATINFO struct _cat_info_strtere_
CATINFO {
    int NumCat, *NumValue;
    double *Average, *Max, *Min;
};

GLOBAL BIGF BigF;
GLOBAL double **Surface, NS, EW, FilterSD, AllMaxDist, *Norm;
GLOBAL int MapCount, FDM, Rs, Cs, Theory;
GLOBAL CELL *CellBuffer;
GLOBAL FILTER *AllFilters, Filter;
GLOBAL CATINFO CatInfo;
GLOBAL int *Seeds, Seed, NumSeeds, Low, High, NumMaps, NumFilters, OutFD;
GLOBAL char Buf[240], **OutNames, *TheoryName, *Mapset;
GLOBAL struct Flag *Uniform;

    /* please, remove before GRASS 7 released */
GLOBAL struct Flag *Verbose;
GLOBAL struct Option *Distance, *Exponent, *Weight;
#endif
