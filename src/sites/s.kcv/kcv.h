/*
 * Copyright (C) 1993-1994. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

struct parm
{
  struct Option *output, *npartitions;
};
typedef struct parm PARM;

struct flag
{
  struct Flag *rand, *drand48, *random;
};
typedef struct flag FLAG;

struct zstruct
{
  int partition;
  double x, y; 
  char desc[80];
};
typedef struct zstruct Z;

struct deestruct
{
  int i;
  double dist;
};
typedef struct deestruct D;

/* dreset.c */
void d_reset(D **, int);
/* histo.c */
int make_histo(int **, int, int);
/* readsite.c */
int readsites(FILE *, int, Z **, struct Cell_head);
/* utils.c */
double myrand(void);
FILE *opensites(char *, int, char *);
int dcmp(void *, void *);
