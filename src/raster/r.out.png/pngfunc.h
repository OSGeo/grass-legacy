#include "pnm.h"
#include "png.h"	/* includes zlib.h and setjmp.h */
#include "version.h"	/* VERSION macro */


typedef struct _jmpbuf_wrapper {
  jmp_buf jmpbuf;
} jmpbuf_wrapper;

/* GRR 19991205:  this is used as a test for pre-1999 versions of netpbm and
 *   pbmplus vs. 1999 or later (in which pm_close was split into two) 
 */
#ifdef PBMPLUS_RAWBITS
#  define pm_closer pm_close
#  define pm_closew pm_close
#endif

#ifndef TRUE
#  define TRUE 1
#endif
#ifndef FALSE
#  define FALSE 0
#endif
#ifndef NONE
#  define NONE 0
#endif
#define MAXCOLORS 256
#define MAXCOMMENTS 256

/* function prototypes */
#ifdef __STDC__
static void pnmtopng_error_handler (png_structp png_ptr, png_const_charp msg);
#endif

static int filter = -1;
static int compression = Z_DEFAULT_COMPRESSION;
static jmpbuf_wrapper pnmtopng_jmpbuf_struct;
