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
    int x = (d <= 0 || s <= 0)
	? 100
	: 100 * n / d;

    if (n <= 0 || n >= d || x > prev + s)
    {
	prev = x;
	fprintf (stderr,"%4d%%\b\b\b\b\b",x);
    }

    if (x >= 100)
    {
	fprintf (stderr,"\n");
	prev = -1;
    }

    return 0;
}
