/* act II ink-jet printer definitions */

#include <stdio.h>

#define LOOKUP  (char) (0x01)   /* load lookup table */
#define SIZE    (char) (0x02)   /* set picture size*/
#define RASTER  (char) (0x03)   /* select raster mode */
#define ALPHA   (char) (0x04)   /* select alpha-numeric mode */
#define REV     (char) (0x05)   /* select black/white reversal */
#define NOREV   (char) (0x06)   /* select no black/white reversal */
#define DATA    (char) (0x07)   /* load data */
#define OFL     (char) (0x08)   /* select printer offline */
                                /* x09 not used */
#define LF      (char) (0x0a)   /* line feed */
#define VT      (char) (0x0b)   /* vertical tab */
#define FF      (char) (0x0c)   /* form feed */
#define CR      (char) (0x0d)   /* carriage return, no effect */
#define ZOOMX   (char) (0x0e)   /* select zoom x factor */
#define ZOOMY   (char) (0x0f)   /* select zoom y factor */
#define ALPHAR  (char) (0x10)   /* select alpha/raster mode */
#define ONL     (char) (0x11)   /* select online, no effect */
#define BICOD   (char) (0x12)   /* bi-directional print */
#define UNICOD  (char) (0x13)   /* uni-directional print */
#define COMPACT (char) (0x14)   /* compact byte mode */
#define ONE_ONE (char) (0x15)   /* 1 to 1 x,y zoom */
#define COLORF  (char) (0x16)   /* alpha foreground color */
#define COLORB  (char) (0x17)   /* alpha background color */
#define ASPECT  (char) (0x18)   /* aspect ratio/resolution */
#define CLEAR   (char) (0x19)   /* clear print buffer */
#define PBAND   (char) (0x1a)   /* print black purge band */
#define ESC     (char) (0x1b)   /* no effect */
#define DLOOKUP (char) (0x1c)   /* load default lookup table */
#define RLE     (char) (0x1d)   /* load data run length encoded */
#define DITHER  (char) (0x95)   /* 2 to 1 x,y zoom */

#define COLOR_WHITE     (char) (0)      /* fore/back ground colors */
#define COLOR_YELLOW    (char) (1)
#define COLOR_MAGENTA   (char) (2)
#define COLOR_RED       (char) (3)
#define COLOR_CYAN      (char) (4)
#define COLOR_GREEN     (char) (5)
#define COLOR_BLUE      (char) (6)
#define COLOR_BLACK     (char) (7)

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL int nrows, ncols, padding ;

#define WHITE (char) 124
