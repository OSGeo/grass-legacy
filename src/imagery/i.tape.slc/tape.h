#include "imagery.h"
#include <math.h>

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int tapefd;
GLOBAL int nbands;
GLOBAL unsigned char *tapebuf;
GLOBAL int tapebufsize;
GLOBAL int bandfd[4];
GLOBAL CELL *cellbuf[4];
GLOBAL struct Range *range[4];
GLOBAL struct Cell_head *cellhd[4];
GLOBAL struct History history[4];
GLOBAL int skipfiles;
GLOBAL int skiprecords;
GLOBAL int bandsize;
GLOBAL int format;
GLOBAL int blocking_factor;
GLOBAL int firstrow, lastrow;
GLOBAL int firstcol, lastcol;
GLOBAL int nrows ;
GLOBAL int bytes_per_pixel;
GLOBAL long headlen;
GLOBAL int bpdg,numrow,numdgprow;
GLOBAL char data_file_name[17];

GLOBAL struct Tape_Info tape_info;

/* advance.c */
int tape_advance(int, int);
/* main.c */
int get_format(void);
int slc(void);
int readslc(int);
int put_row(int, unsigned char *);
/* mount_tape.c */
int mount_tape(char *);
/* tapename.c */
int get_tapename(char *);
