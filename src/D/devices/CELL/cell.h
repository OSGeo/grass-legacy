#ifndef BUFSIZ
#include <stdio.h>
#endif

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
