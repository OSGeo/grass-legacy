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
GLOBAL int reclen,bpdg,numrow,numdgprow;
GLOBAL long recseqnum;
GLOBAL char data_file_name[17];

GLOBAL struct Tape_Info tape_info;

GLOBAL char *voldirname;
GLOBAL char *leadername;
GLOBAL char *dataname;
GLOBAL char *output;
GLOBAL FILE *fin;
GLOBAL float tln,tle,trn,tre,bln,ble,brn,bre;
GLOBAL int zone;
GLOBAL long position;

int get_format(void);
int pri(void);
int readpri(int);
int put_row(int, unsigned char *);
void ini_pos(FILE *);
