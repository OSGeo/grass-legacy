/* %W%  %G% */

#include <stdio.h>

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows,ncols,nbytes;
GLOBAL int isinit,alphamd,rastermd;
GLOBAL int ndots;
GLOBAL int pict_row;
GLOBAL FILE *yellow_f;
GLOBAL FILE *magenta_f;
GLOBAL FILE *cyan_f;
GLOBAL FILE *caltxt_f;
GLOBAL FILE *temp_f;
GLOBAL unsigned char plotfont[95][40];

#define RASTER (char) (0x02)
#define EOT    (char) (0x04)
#define LF     (char) (0x0a)
#define FF     (char) (0x0c)
#define CR     (char) (0x0d)
#define ESC    (char) (0x1b)
#define ERROR          0
#define NDOTSV (int) 2000     /* No. of pixels vertically  */
#define CHARHGT (int)  20     /* height of char. in dots   */
#define LINSPAC (int)  10     /* line spacing in dots      */
#define LM      (int)  28     /* left margin,discard 8bytes*/
#define DELAYTIME 500         /* for proper plot operation */
#define NCOLBITS (int) 3      /* 2**3 colours */
/* font for 0x20 to 0x3f */
#define FONTF1  "/usr/grass3/src/paint/Drivers/calcomp1/font1"
/* font for 0x40 to 0x5f */
#define FONTF2  "/usr/grass3/src/paint/Drivers/calcomp1/font2"
/* font for 0x60 to 0x7e */
#define FONTF3  "/usr/grass3/src/paint/Drivers/calcomp1/font3"

