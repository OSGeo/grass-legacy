#include <gd.h>

#ifndef BUFSIZ
#include <stdio.h>
#endif

#include "config.h"

#define DEF_WIDTH  640
#define DEF_HEIGHT 480

#if defined(HAVE_GDIMAGEPNG)
# define FILE_NAME  "map.png"
#elif defined(HAVE_GDIMAGEGIF)
# define FILE_NAME  "map.gif"
#else
# error Neither PNG nor GIF supported
#endif

extern char *file_name;
extern FILE *output;
extern gdImagePtr im;
extern int currentColor;
extern unsigned long *xpixels;
extern int true_color;
extern int NCOLORS;
