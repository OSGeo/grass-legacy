/* Function: text_box_path
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"

#define LEFT 0
#define RIGHT 1
#define LOWER 0
#define UPPER 1
#define CENTER 2

text_box_path(x, y, xref, yref, text, fontsize)
int x, y, xref, yref, fontsize;
char *text;
{
    /* get relative box coordinates */
    fprintf(PS.fp, "ZB (%s) PB\n", text);

    /* set box x coordinate */
    fprintf(PS.fp, "%d ", x);
    switch (xref)
    {
   	case LEFT: fprintf(PS.fp, "LTX");  break;
	case RIGHT: fprintf(PS.fp, "CTX"); break;
	case CENTER:
	default:
		fprintf(PS.fp, "CTX"); break;
    }

    /* set box y coordinate */
    fprintf(PS.fp, " %d ", y);
    switch (yref)
    {
	case UPPER: fprintf(PS.fp, "UTY");  break;

	case LOWER: fprintf(PS.fp, "LTY");  break;

	case CENTER:
	default:
		fprintf(PS.fp, "CTY"); break;
    }
    fprintf(PS.fp, " TR TB\n");
}

