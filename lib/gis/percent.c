#include "gis.h"
/******************************************************************
* G_percent (n, d, s)
*
*  print a counter to stderr
*  prints percentage of n in d, in increments of s
*
*  example:
*
*       for (row = 0; row < nrows; row++)
*       {
*           G_percent (row, nrows, 10);
*                ...
*       }
*       G_percent (row, nrows, 10);
*
*  will print percent complete for row/nrows in multiples of 10  
*****************************************************************/ 
#include <stdio.h>

static int prev = -1;
static int first = 1;


/*!
 * \brief print percent complete messages
 *
 * This routine prints a percentage complete message to stderr. The
 * percentage complete is (<b>n</b>/ <b>total</b>)*100, and these are printed
 * only for each <b>incr</b> percentage. This is perhaps best explained by
 * example:
\code
  #include <stdio.h>
  int row;
  int nrows;
  nrows = 1352; // 1352 is not a special value - example only
  fprintf (stderr, ''Percent complete: '');
  for (row = 0; row < nrows; row++)
  G_percent (row, nrows, 10);
\endcode
 * This will print completion messages at 10% increments; i.e., 10%, 20%, 30%,
 * etc., up to 100%. Each message does not appear on a new line, but rather erases
 * the previous message. After 100%, a new line is printed.
 *
 *  \param n
 *  \param total
 *  \param incr
 *  \return int
 */

int G_percent (int n,int d,int s)
{
    return ( G_percent2 ( n, d, s, stderr ) );
}


/*!
 * \brief print percent complete messages
 *
 * This routine prints a percentage complete message to stderr. The
 * percentage complete is (<b>n</b>/ <b>total</b>)*100, and these are printed
 * only for each <b>incr</b> percentage. This is perhaps best explained by
 * example:
\code
  #include <stdio.h>
  int row;
  int nrows;
  nrows = 1352; // 1352 is not a special value - example only
  fprintf (stderr, ''Percent complete: '');
  for (row = 0; row < nrows; row++)
  G_percent (row, nrows, 10);
\endcode
 * This will print completion messages at 10% increments; i.e., 10%, 20%, 30%,
 * etc., up to 100%. Each message does not appear on a new line, but rather erases
 * the previous message. After 100%, a new line is printed.
 *
 *  \param n
 *  \param total
 *  \param incr
 *  \param out print here
 *  \return int
 */

int G_percent2 (int n,int d,int s, FILE *out)
{
    int x, format;

    format = G_info_format ();

    x = (d <= 0 || s <= 0)
	? 100
	: 100 * n / d;
	

    if (n <= 0 || n >= d || x > prev + s)
    {
	prev = x;
        
	if ( format == G_INFO_FORMAT_STANDARD ) {
	    if ( out != NULL ) {
	        fprintf (out,"%4d%%\b\b\b\b\b",x);
	    }
	} else { /* GUI */
	    if ( out != NULL ) {
		if ( first ) {
		    fprintf (out,"\n");
		}
		fprintf (out,"GRASS_INFO_PERCENT: %d\n", x);
		fflush ( out );
	    }
	    first = 0;
	}
    }

    if (x >= 100)
    {
	if ( format == G_INFO_FORMAT_STANDARD ) {
	    if ( out != NULL ) {
	        fprintf (out,"\n");
	    }
	}
	prev = -1;
	first = 1;
    }

    return 0;
}
