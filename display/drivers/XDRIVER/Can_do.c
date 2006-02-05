#include "XDRIVER.h"

/* Returns 0 (no) or 1 (yes) to indicate whether or not a floating
 * color table is available. */

extern int truecolor;

int XD_Can_do_float(void)
{
	return !truecolor;
}

