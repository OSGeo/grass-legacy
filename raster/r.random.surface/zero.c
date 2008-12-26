/* zero.c                                                               */

#undef TRACE
#undef DEBUG

#undef MAIN
#include "ransurf.h"


void ZeroMapCells(void)
{
    int Row, Col;

    G_debug(2, "ZeroMapCells()");

    for (Row = 0; Row < Rs; Row++) {
	for (Col = 0; Col < Cs; Col++)
	    Surface[Row][Col] = 0.0;
    }
}
