/* %W% %G% */
#include "gis.h"

    EXTERN FILE *output_fd;
    EXTERN struct Cell_head window;
    EXTERN int nrows;
    EXTERN int ncols;
    EXTERN float Ktable[11];
    EXTERN int PItable[11];
    EXTERN float Doftable[11];
    EXTERN float Kwtable[11];
    EXTERN float Yctable[11];
    EXTERN float Portable[11];
    EXTERN float fin1table[11];
    EXTERN float fin2table[11];

    EXTERN struct subws
    {
        int count;
        double slope;
        int soils[12];
        int cover[4];
    };

    EXTERN struct subws *watshed;

    EXTERN int soils_flag;
    EXTERN int cover_flag;
    EXTERN int nodes_flag;
