#include <stdio.h>

#define COLORBYTES 256
typedef unsigned char COLOR[2][COLORBYTES];


#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols ;
GLOBAL int ras_row;
GLOBAL int ras_nrows;
GLOBAL COLOR YELLOW;
GLOBAL COLOR CYAN;
GLOBAL COLOR MAGENTA;
GLOBAL char cyanfile[30], magentafile[30], yellowfile[30];
GLOBAL int cyanfd, magentafd, yellowfd;

#define TEXT_ROWS 20
#define TEXT_COLS 1024
#define TEXT_MIDDLE 10
#define TEXT_SIZE 2.0
GLOBAL char textbuf[TEXT_ROWS][TEXT_COLS];

#define GS ( (char) 0x1d )
#define VT ( (char) 0x0b )
#define FF ( (char) 0x0c )
