#include "imagery.h"

#define BIL 1
#define BSQ1 2
#define BSQ2 3

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int tapefd;
GLOBAL int *bandfd;
GLOBAL int *wantband;
GLOBAL int *first;
GLOBAL int nbands;
GLOBAL unsigned char *tapebuf;
GLOBAL int tapebufsize;
GLOBAL CELL *cellbuf;
GLOBAL int skipfiles;
GLOBAL int skiprecords;
GLOBAL int skipbytes;
GLOBAL int bandsize;
GLOBAL int format;
GLOBAL int blocking_factor;
GLOBAL int firstrow, lastrow;
GLOBAL int firstcol, lastcol;
GLOBAL int nrows;
GLOBAL int ncols;

GLOBAL struct Tape_Info tape_info;

/* advance.c */
int tape_advance(int, int);
/* ask_format.c */
int ask_format(void);
/* ask_window.c */
int ask_window(void);
/* bil.c */
int bil(void);
/* bsq1.c */
int bsq1(void);
/* bsq2.c */
int bsq2(void);
/* mount_tape.c */
int mount_tape(char *);
/* put_row.c */
int put_row(int, unsigned char *);
/* readbil.c */
int readbil(int, int);
/* readbsq.c */
int readbsq(int);
/* tape_advance.c */
int tape_advance(int, int);
/* tapename.c */
int get_tapename(char *);
