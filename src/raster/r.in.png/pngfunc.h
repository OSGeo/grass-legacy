#define VERSION "2.37.3 (13 July 1999)"

/* GRR 19990713:  fixed redundant freeing of png_ptr and info_ptr in setjmp()
 *  blocks and added "pm_close(ifp)" in each.  */

/* GRR 19990317:  declared "clobberable" automatic variables in convertpng()
 *  static to fix Solaris/gcc stack-corruption bug.  Also installed custom
 *  error-handler to avoid jmp_buf size-related problems (i.e., jmp_buf
 *  compiled with one size in libpng and another size here).  */

#include "png.h"
#include "pnm.h"
#include "zlib.h"
#include "setjmp.h"

#ifndef TRUE
#  define TRUE 1
#endif
#ifndef FALSE
#  define FALSE 0
#endif
#ifndef NONE
#  define NONE 0
#endif

/* function prototypes */
#ifdef __STDC__
static png_uint_16 _get_png_val (png_byte **pp, int bit_depth);
static void store_pixel (xel *pix, png_uint_16 r, png_uint_16 g, png_uint_16 b,
                         png_uint_16 a);
static int iscolor (png_color c);
static void save_text (png_info *info_ptr, FILE *tfp);
static void show_time (png_info *info_ptr);
static void pngtopnm_error_handler (png_structp png_ptr, png_const_charp msg);
int main (int argc, char *argv[]);
#endif

typedef struct _jmpbuf_wrapper {
  jmp_buf jmpbuf;
} jmpbuf_wrapper;

enum alpha_handling
  {none, alpha_only, mix};
  
static png_uint_16 maxval, maxmaxval;
static png_uint_16 bgr, bgg, bgb; /* background colors */
static int verbose = FALSE;
static enum alpha_handling alpha = none;
static int background = -1;
static char *backstring;
static float displaygamma = -1.0; /* display gamma */
static float totalgamma = -1.0;
static int text = FALSE;
static char *text_file;
static int mtime = FALSE;
static jmpbuf_wrapper pngtopnm_jmpbuf_struct;
