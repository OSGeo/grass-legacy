#include <stdio.h>

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols ;
GLOBAL int rasterfd;
GLOBAL char *rasterfile;
GLOBAL char *sprint_command;
GLOBAL int WHITE, BLACK;
GLOBAL unsigned char *data_buf;

#define TEXT_ROWS 20
#define TEXT_COLS 1000
#define TEXT_MIDDLE 10
#define TEXT_SIZE 2.0
GLOBAL char textbuf[TEXT_ROWS][TEXT_COLS];
