#include <grass/gis.h>
#ifdef MAIN
  struct Quant quant_struct;
  CELL old_min, old_max;
  DCELL old_dmin, old_dmax;
  char *name[100];  /* input map names */
  char *mapset[100];  /* input mapsets */
  int noi;
#else
  extern struct Quant quant_struct;
  extern CELL old_min, old_max;
  extern DCELL old_dmin, old_dmax;
  extern char *name[100];  /* input map names */
  extern char *mapset[100];  /* input mapsets */
  extern int noi;
#endif
/* read_rules.c */
int read_range(void);
int report_range(void);
int read_rules(void);
