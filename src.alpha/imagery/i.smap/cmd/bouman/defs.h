#include <math.h>
#include <string.h>
#include "gis.h"

#define LIKELIHOOD float

/* Subroutines in interp.c */
void seq_MAP();
void MLE();

/* Subroutines in decimate.c */
void make_pyramid();
char ***get_pyramid();
void free_pyramid();
char ****get_cubic_pyramid();
void free_cubic_pyramid();

/* Subroutines in reg_util.c */
int levels_reg();
void dec_reg();
void copy_reg();
void reg_to_wdht();

/* Subroutines in model.c */
void extract();
void extract_init();

/* Subroutines in multialloc.c */
char *multialloc();
void multifree();
unsigned char **get_img();
void free_img();

/* Subroutine in solve.c */
double  solve();

/* Subroutine in eigen.c */
int eigen();

/* Subroutine in invert.c */
int invert();
