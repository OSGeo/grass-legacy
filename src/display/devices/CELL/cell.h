#include <stdio.h>

#include "driver.h"
#include "driverlib.h"

#define FILE_NAME "D_cell"

extern unsigned char Cur_color;
extern char *Filename;
extern FILE *Temp_fp;
extern unsigned char Color_table[256][3];
extern unsigned char *Row_buf;

/* bresline.c */
int bres_line(int, int, int, int);
/* file_io.c */
int store_xy(int, int);
int horiz_line(int, int, int);
/* polyfill.c */
int polyfill(int *, int *, int, int (*)(int,int,int));
