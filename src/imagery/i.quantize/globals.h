#include "imagery.h"

#ifndef GLOBAL
# define GLOBAL extern
#endif

GLOBAL int *RED;
GLOBAL int *GRN;
GLOBAL int *BLU;

GLOBAL int r_level;
GLOBAL int g_level;
GLOBAL int b_level;

GLOBAL struct Ref ref;

GLOBAL char group[40], result[40];

/* ask_levels.c */
int ask_levels(void);
/* compose.c */
int compose(void);
int valid_range(struct Range *);
int readrgbline(CELL *, CELL *, CELL *);
int writemappedline(CELL *);
int rewind_input(void);
int setcmap(int, unsigned char *, unsigned char *, unsigned char *);
int open_output_file(void);
int close_output_file(void);
int imagewidth(void);
int imageheight(void);
/* gen_colors.c */
int gen_colors(void);
/* median.c */
int *salloc(int);
int median(int);
int usage(void);
