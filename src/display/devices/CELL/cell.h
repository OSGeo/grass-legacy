#ifndef BUFSIZ
#include <stdio.h>
#endif

#include "driverlib.h"

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define DEF_WIDTH  640
#define DEF_HEIGHT 480
#define FILE_NAME "D_cell"


GLOBAL unsigned char Cur_color;
GLOBAL char *Filename;
GLOBAL FILE *Temp_fp;
GLOBAL unsigned char Color_table[256][3];
GLOBAL unsigned char *Row_buf;

/* bresline.c */
int bres_line(int, int, int, int);
/* file_io.c */
int store_xy(int, int);
int horiz_line(int, int, int);
/* polyfill.c */
int polyfill(int *, int *, int, int (*)(int,int,int));
