#include <gd.h>

#include "config.h"
#include "driver.h"

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
