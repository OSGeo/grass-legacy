#include <gd.h>

#ifndef BUFSIZ
#include <stdio.h>
#endif

#include "config.h"

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define DEF_WIDTH  640
#define DEF_HEIGHT 480

#if defined(HAVE_GDIMAGEPNG)
# define FILE_NAME  "map.png"
#elif defined(HAVE_GDIMAGEGIF)
# define FILE_NAME  "map.gif"
#else
# error Neither PNG nor GIF supported
#endif

GLOBAL char *file_name;
GLOBAL FILE *output;
GLOBAL gdImagePtr im;
GLOBAL int currentColor;
