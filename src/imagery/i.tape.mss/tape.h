#include "imagery.h"


#define BIL		1
#define BSQ		2
#define UNKNOWN		0
#define CORRECTED	1
#define UNCORRECTED	2

#define REC_SIZE	4096
#define HAVE_IMAGE	0
#define NO_HEADER	-1

#define TAPE_DIR	011
#define HEADER		022
#define ANCILLARY	044
#define ANNOTATION	0333
#define IMAGE		0355
#define TRAILER		0366
#define REC_SIZE	4096

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int record_size;
GLOBAL int tape_type;
GLOBAL int correction;
GLOBAL int vol;
GLOBAL int nbands;
GLOBAL char date[30];
GLOBAL char sun_angle[30];
GLOBAL char mission[30];
GLOBAL char scene_id[40];
GLOBAL int *want_band;
GLOBAL int *bandfd;
GLOBAL struct Tape_Info tape_info;
GLOBAL int firstcol, lastcol, firstrow, lastrow;
GLOBAL CELL *cellbuf;
/* annotation.c */
int annotation(unsigned char []);
/* ask_info.c */
int ask_info(void);
/* ask_window.c */
int ask_window(int, int);
/* extract.c */
int extract(int, unsigned char [], int, int, int, int, int, int);
/* header.c */
int header(unsigned char []);
/* image_fmt.c */
int image_fmt(unsigned char [], int *, int *, int *);
/* main.c */
int main(int, char *[]);
/* mount_tape.c */
int mount_tape(char *);
/* put_row.c */
int put_row(int, unsigned char *, int);
/* read_tape.c */
int read_tape(int, unsigned char [], int);
/* tape_dir.c */
int tape_dir(unsigned char []);
/* tape_vol.c */
int tape_vol(int);
/* tapename.c */
int get_tapename(char *);
